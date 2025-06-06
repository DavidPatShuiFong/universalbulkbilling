# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

library(shiny)
library(bslib)
library(bsicons)
library(tibble)
library(plotly)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(rhandsontable)
library(htmlwidgets)
library(shinyalert)
library(tools)
library(readxl)

adapt_csv_table <- function(df) {
  df |>
    mutate(
      # create proportion columns (0 to 1) from raw proportion data
      service_proportion_bulk = service_proportion_raw_bulk/sum(service_proportion_raw_bulk),
      service_proportion_private = service_proportion_raw_private/sum(service_proportion_raw_private),
      # fix_gap is used to fix the gap fee when drawing plots
      fix_gap = FALSE
    ) |>
    select(
      fee_names, service_fees, incentive_by_fee = individual_bulkbill_incentive_by_fee,
      gap_fee, fix_gap,
      service_proportion_bulk, service_proportion_private,
      service_proportion_raw_bulk, service_proportion_raw_private
    )
}

service_items <- read.csv("./medicarebenefits.csv") |>
  adapt_csv_table()

# the individual service bulk-bill incentive (dollars) varies by location
#   Modified Monash areas 1, 2, 3+4, 5, 6, 7
#   we will index the above to 1, 2, 3, 4, 5 and 6 (merging areas 3+4)

# see https://www9.health.gov.au/mbs/search.cfm
n_monash <- 6 # six different Monash areas
individual_bulkbill_incentive <- array(
  # there are two different individual bulk-billing incentive fees,
  # 'standard' and 'triple'
  data = c(
    # standard bulk-billing incentive applies to service items like item A/'3'
    # items 10990, 10991, 75855, 75856, 75857, 75858
    c(7.15, 10.80, 11.45, 12.20, 12.85, 13.70),
    # triple bulk-billing incentive applies to service items like item B, C and D
    # items 75870, 75871, 75873, 75874, 75875, 75876
    c(21.35, 32.50, 34.50, 36.65, 38.70, 41.10)
  ),
  dim = c(n_monash, 2),
  dimnames = list(c("1", "2", "3+4", "5", "6", "7"), c("single", "triple"))
)

# universal bulk billing incentive 'loading payment'
# over the service fee. paid quarterly
# set to 12.5%
universal_bulkbill_incentive <- 0.125

fee_mean <- function(
  # calculate the current mean fee
  service_fees,
  fee_names,
  service_proportion_bulk,
  service_proportion_private,
  gap_fee,
  concessional_bulkbilled,
  individual_bulkbill_incentive,
  individual_bulkbill_incentive_by_fee,
  monash_area
) {
  # calculate individual bulk-bill incentive applicable to defined Monash area
  fee_bulkbill_incentive <-
    individual_bulkbill_incentive[monash_area, individual_bulkbill_incentive_by_fee]
  # re-define dimensions of the 'x' array
  dim(fee_bulkbill_incentive) <- c(length(fee_names), 1)
  # change row name to the fee names
  rownames(fee_bulkbill_incentive) <- fee_names

  (1 - concessional_bulkbilled) * sum(service_fees * service_proportion_private) +
    concessional_bulkbilled * sum(service_fees * service_proportion_bulk) +

    # calculate (and add) the individual service bulk-billing incentive applied
    # to patients currently concessionally bulk-billed
    concessional_bulkbilled * sum(service_proportion_private * fee_bulkbill_incentive) +

    # calculate (and add) the private 'gap' fee
    (1 - concessional_bulkbilled) * sum(service_proportion_private * gap_fee)
}

net_benefit <- function(
    # calculate the average net benefit revenue
  # to the practice per patient

  # the variable names used in this function
  # are the same as used in previous code chunks
  # but will be 'local' to this function
  service_fees,
  fee_names,
  service_proportion_bulk,
  service_proportion_private,
  gap_fee,
  concessional_bulkbilled,
  individual_bulkbill_incentive,
  individual_bulkbill_incentive_by_fee,
  monash_area,
  universal_bulkbill_incentive) {
  # calculate individual bulk-bill incentive applicable to defined Monash area
  fee_bulkbill_incentive <- individual_bulkbill_incentive[monash_area, individual_bulkbill_incentive_by_fee]
  # re-define dimensions of the 'x' array
  dim(fee_bulkbill_incentive) <- c(length(fee_names), 1)
  # change row name to the fee names
  rownames(fee_bulkbill_incentive) <- fee_names

  # calculate the universal bulk-bill incentive
  # for patients formerly privately-billed
  (1 - concessional_bulkbilled) * sum(service_fees * service_proportion_private) *
    universal_bulkbill_incentive +
    # calculate (and add) benefit for patients already concessionally bulk-billed
    concessional_bulkbilled *  sum(service_fees * service_proportion_bulk) *
    universal_bulkbill_incentive +

    # calculate (and add) the individual service bulk-billing incentive applied
    # to patients formerly privately-billed
    (1 - concessional_bulkbilled) * sum(service_proportion_private * fee_bulkbill_incentive) -

    # calculate (and subtract) the loss from not charging a private 'gap' fee
    (1 - concessional_bulkbilled) * sum(service_proportion_private * gap_fee)
}


# Define UI for application that draws a histogram
ui <- page_sidebar(
  # make horizontal rules more visible
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),

  # Application title
  title = "Universal bulk-billing benefit-loss",
  theme = bs_theme(bootswatch = "journal"),

  # Sidebar with a slider input for number of bins
  sidebar = sidebar(
    width = "30%",
    position = "left",
    selectInput(
      "monash",
      "Monash area",
      dimnames(individual_bulkbill_incentive)[[1]]
    ),
    hr(),
    h4(em("Quick calculator")),
    sliderInput(
      inputId = "concessional_bulkbilled",
      label = "Concessional bulk-billed (%)",
      min = 0,
      max = 100,
      value = 50
    ),
    em("Proportion of services which are both currently bulk-billed AND receive bulk-bill incentives e.g. have a concession card"),
    sliderInput(
      inputId = "all_gapfees",
      label = "Set mean of ALL gap fees ($)",
      min = 0,
      max = 100,
      value = 50
    ),
    checkboxInput(
      inputId = "all_gapfees_confirm",
      label = "Enable to set ALL mean gap fees (except those services marked 'fix_gap')"
    ),
    em("The mean gap-fee for all services which were either not bulk-billed or charged without receiving current bulk-bill incentives."),
    layout_column_wrap(
      uiOutput("mean_fee_valuebox"),
      uiOutput("simple_benefit_valuebox"),
      uiOutput("simple_benefit_relative_valuebox")
    ),
    div(
      em("By default, all calculations ('simple' and the tables and plots) are based on 'average' MBS service item distribution"),
      em("according to MBS statistics, 2024. MBS service item distribution can be changed through"),
      em("the"), actionLink("jump_to_mbs_schedule", em("Medicare Benefits Schedule")), em(" tab.")
    ),
    hr(),
    accordion_panel(
      "Themer", icon = bsicons::bs_icon("palette"),
      br(),
      selectInput(
        inputId = "theme_choice",
        label = "Choose a Bootstrap theme:",
        choices = bootswatch_themes(),
        selected = "journal" # Default theme
      )
    ),
    hr(),
    tags$footer(
      "Dr David Fong", br(),
      icon("github"),
      tags$a(
        "Source code",
        target = "_blank",
        href = "https://github.com/DavidPatShuiFong/universalbulkbilling"
      ), "- review welcome", br(),
      icon("globe"), "Explanatory notes - ",
      tags$a(
        "www.davidfong.org",
        target = "_blank",
        href = "http://www.davidfong.org/post/universalbulkbilling-update/"
      ), br(),
      "🄯",
      tags$a(
        "Mozilla Public License 2.0",
        target = "_blank",
        href = "https://www.mozilla.org/en-US/MPL/2.0/"
      ), br(),
      "Free for private use, Free for public benefit",
      style = "width: 100%; color: black; text-align: center;"
    )
  ),
  # Show a plot of the generated distribution
  navset_card_underline(
    title = "Plots",
    id = "panels",
    nav_panel(
      "Benefit-Loss",
      card(
        tags$div(
          style = "min-width: 80ch; width: 100%;",
          plotlyOutput("ggplot_benefitloss")
        ),
        min_height = 450
      ),
      div(
        "Calculated net benefit per service item if proposed universal bulk-billing adopted,",
        "across a range of pre-existing proportion of patients bulk-billed (with existing bulk-billing incentives)",
        "and mean gap fees charged for all other services (which would have qualified for bulk-billing incentive if the patient currently has a health care card etc., or will qualify for all patients under the universal bulk-billing proposal).",
        br(), hr(),
        downloadButton("download", "Download benefits '.csv' or Excel table"), br(), br(),
        "Downloaded table format:", br(),
        em("gap"),"- mean gap fee ($) for services which are billed to a patient without receiving a bulk-billing incentive (e.g. service for an adult without a concession card).",
        "If these services were 'bulk-billed' to a patient with a concession card, then the service would attract a bulk-billing incentive.",
        "Note that the 'mean' gap fee for these services includes services which are 'bulk-billed to patient who do not qualify for current bulk-billing incentives.", br(),
        em("concessional_bulkbilled"), "- proportion (%) of services which are bulk-billed and receive a bulk-bill incentive payment.",
        "This is a proportion of *all* services charged with these service item numbers.",
        "Only services which potentially attract bulk-billing incentives should be included.", br(),
        em("current_fee_mean"), "- the calculated current mean fee for services.", br(),
        em("benefit"), "- the mean benefit/cost per service of adopting universal bulk-billing.", br(),
        em("benefit_rel"), "- the calculated change in revenue of adopting universal bulk-billing."
      )
    ),
    nav_panel(
      "Relative benefit",
      br(),
      card(
        tags$div(
          style = "min-width: 80ch; width: 100%;",
          plotlyOutput("ggplot_benefitrelative")
        ),
        min_height = 450
      ),
    ),
    nav_panel(
      "3D Plot",
      br(),
      card(
        tags$div(
          style = "min-width: 80ch; width: 100%",
          plotlyOutput("plotly_3d", height = "70vh")
        ),
        min_height = 450,
        br(),
        layout_columns(
          col_width = c(-1, 4, 4, -1),
          selectInput(
            inputId = "zaxis_plotly3d",
            label = "Z-axis variable",
            choices = c("Benefit ($)", "Revenue change (%)")
            ),
          selectInput(
            inputId = "table_resolution",
            label = "Resolution of table",
            choices = c("$5 and 5%" = 5, "$2.5 and 2.5%" = 2.5),
          )
        )
      )
    ),
    nav_panel(
      "Medicare Benefit Schedule",
      h3("Medicare Benefit Schedule table"),
      tags$div(id = "mbs_table_placeholder"),
      br(),
      tabsetPanel(
        tabPanel(
          "Upload MBS description table",
          br(),
          fileInput(
            "upload_mbs",
            "Choose MBS description table (.csv or Excel)",
            accept = c(".csv", ".xls", ".xlsx"),
            width = "50%"
          ),
          div(
            "Upload your own Medicare Benefits description table!",
            "For examples, see ",
            tags$a(
              "'.csv' files on", icon("github"), "Github,",
              target = "_blank",
              href = "https://github.com/DavidPatShuiFong/universalbulkbilling/"
            ),
            "and column description below."
          ),
          div(
            "Must include columns:",
            em("fee_names, service_fees, service_proportion_raw_bulk, service_proportion_raw_private, gap_fee, individual_bulkbill_incentive_by_fee"), "."
          ),
        ),
        tabPanel(
          "Set fix_gap for all services",
          br(),
          checkboxInput(
            inputId = "fix_all_gap_fees_confirm",
            label = "Set fix_gap for all services",
            FALSE
          ),
          radioButtons(
            inputId = "fix_all_gap_fees",
            label = NULL,
            inline = TRUE,
            choiceNames = c("Off", "On"),
            choiceValues = c(FALSE, TRUE)
          ),
          br(),
          div("See description of", em("fix_gap"), "under 'Column description' below")
        )
      ),
      hr(),
      h3("Table editing"),
      div(
        "By default, the table includes 'raw' numbers from Medicare Benefits Schedule statistics, 2024.",
        "*Only* service items to which bulk-billing incentives apply should be included in this table.",
        "Using 'right-click', MBS service item rows can be added or removed."
      ),
      h4("Column description"),
      div(em("fee_names"), "- Name of MBS service item. *Only* service items to which bulk-billing incentives apply should be included in the table"),
      div(em("service_fees"), "- Medicare Benefit Schedule rebate, excluding incentives"),
      div(em("incentive_by_fee"), "- whether single- or triple- bulk-billing incentive applies"),
      div(em("gap_fee"), "- ", "the mean gap fee charged when the service is not bulk-billed or the service has not received a bulk-bill incentive"),
      div(em("fix_gap"), "- If ticked, then this service fee is 'fixed' to the 'gap' value when plotting tables and graphs, instead of varying from $0 to $70.",
        "Services which are marked as 'fix_gap' are also not altered by the 'set mean of all gap fees' adjustment tool in the 'Quick Calculator'.",
        "This might be useful if the clinic doesn't ever charge 'gap' fees for certain items e.g. health assessments."),
      div(
        em("service_proportion_raw_bulk"), "and", em("service_proportion_raw_private"),
        "- the 'raw' number/proportion which the service item contributes to either",
        "'bulk-billed with incentive' services or 'other/private' services.",
        "For simplicity, the same number can be used in both 'bulk' and 'private' columns.",
        "From the 'raw' numbers, a proportion is calculated (the calculated columns, 'service_proportion_bulk' and 'service_proportion_private', cannot be directly edited)."
      ),
    ),
    nav_panel(
      "Equations",
      htmlOutput("formulae")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  values = reactiveValues(
    service_table = service_items,
    build_mbs_table = TRUE # will need to build service table
  )
  # multiple tables could be created and destroyed
  # unfortunately, shiny doesn't allow destruction of input$xxx
  # which creates problems when trying to re-read the MBS rhandsonable from CSV
  # as, by default, the rhandsonable attempts to read the input$xxx of the
  # previously 'removed' table!
  # mbs_table_version allows each created rhandsonable to have a different name,
  # thereby avoiding the problem of the previous input$xxx still existing
  mbs_table_version <- 1

  addResourcePath("www", getwd())
  output$formulae <- renderUI({
    tags$iframe(
      seamless = "seamless",
      src = "www/formula.html",
      style = "width:100%; height:80vh;"
    )
  })

  # change theme according to user choice
  observe(session$setCurrentTheme(
    bs_theme(bootswatch = input$theme_choice)
  ))

  # the proportion (0 - 1) of patients who are both bulk-billed
  # AND currently qualify for the bulk-billing incentives
  concessional_bulkbilled <- reactive({
    # change from percentage to (0-1) proportion
    input$concessional_bulkbilled/100
  }) |>
    bindEvent(input$concessional_bulkbilled)

  observe({
    nav_select("panels", "Medicare Benefit Schedule")
  }) |>
    bindEvent(input$jump_to_mbs_schedule)

  observe({
    req(input$upload_mbs)

    ext <- file_ext(input$upload_mbs$name)
    data <- switch(
      ext,
      csv = read.csv(input$upload_mbs$datapath),
      xls = read_excel(input$upload_mbs$datapath),
      xlsx = read_excel(input$upload_mbs$datapath),
      validate("Invalid file: please upload a '.csv'/Excel file")
    )
    if (
      !all(
        c(
          "fee_names",
          "service_fees",
          "service_proportion_raw_bulk",
          "service_proportion_raw_private",
          "gap_fee",
          "individual_bulkbill_incentive_by_fee"
        )
        %in% colnames(data)
      )
      ) {
        shinyalert(
          "Invalid spreadsheet",
          "Must contain columns fee_names, service_fees, service_proportion_raw_bulk, service_proportion_raw_private, gap_fee and individual_bulkbill_incentive_by_fee",
          type = "error"
        )
        return()
      }

    # copy to MBS service table after adaptation
    # first remove MBS service table display
    output[[paste0("mbs_table_", mbs_table_version)]] <- NULL
    removeUI(selector = paste0("#mbs_table_", mbs_table_version), immediate = TRUE)
    # then set values and prompt a MBS service table rebuild
    values[["service_table"]] <- adapt_csv_table(data)
    values[["build_mbs_table"]] <- TRUE
    # increment mbs table version number (global variable)
    mbs_table_version <<- mbs_table_version + 1
  }) |>
    bindEvent(input$upload_mbs)

  output$download <- downloadHandler(
    filename = function() {
      paste("UniversalBulkBill Benefits ", Sys.Date(), ".csv", sep = "")
      },
    content = function(file) {
      write.csv(
        x = benefit_loss(),
        file = file,
        row.names = FALSE
      )
    },
    contentType = "text/csv"
  )

  trimmed_service_table <- reactive({
    values[["service_table"]] |>
      # get rid of any rows with empty values
      filter(!is.na(fee_names) & !is.na(service_fees) &
               !is.na(incentive_by_fee) & !is.na(gap_fee) &
               !is.na(service_proportion_bulk) &
               !is.na(service_proportion_private))
  }) |>
    bindEvent(values[["service_table"]])

  plot_service_table <- reactive({
    trimmed_service_table() |>
      # the plots do not need raw service figures
      select(-service_proportion_raw_bulk, -service_proportion_raw_private)
  }) |>
    bindEvent(trimmed_service_table())

  benefit_loss <- reactive({
    # a tibble/dataframe of benefit-loss through a range of
    # mean gap fees and concessional bulkbilling proportions
    benefit_loss <- tibble(
      gap = numeric(),
      concessional_bulkbilled = numeric(),
      current_fee_mean = numeric(),
      benefit = numeric(),
      benefit_rel = numeric()
    )

    # mean gap fees from $0 to $60
    gap_fee_range <- seq(
      from = 0, to = 70,
      by = as.numeric(input$table_resolution)
    )
    # concessional bulk-billed range from 0 to 100%
    concessional_bulkbilled_range <- seq(
      from = 0, to = 100,
      by = as.numeric(input$table_resolution)
    )

    for (x in gap_fee_range) {
      for (y in concessional_bulkbilled_range) {
        new_gap_fee <- ifelse(
          # if the gap-fee is set to 'fix_gap' then set to the gap fee in the table
          # otherwise the mean gap fee is charged for all other items
          #
          # by *default* 'fix_gap is set to FALSE for all items
          # so this tibble will change all the mean gap fees for every item
          plot_service_table()$fix_gap,
          plot_service_table()$gap_fee,
          x
        )
        benefit <- net_benefit(
          service_fees = plot_service_table()$service_fees,
          fee_names = plot_service_table()$fee_names,
          service_proportion_bulk = plot_service_table()$service_proportion_bulk,
          service_proportion_private = plot_service_table()$service_proportion_private,
          gap_fee = new_gap_fee,
          # set concessional bulk-billed proportion to 'x'
          # divide by 100 to convert from
          # percentage to proportion
          concessional_bulkbilled = y/100,
          individual_bulkbill_incentive,
          individual_bulkbill_incentive_by_fee = plot_service_table()$incentive_by_fee,
          input$monash,
          universal_bulkbill_incentive
        )
        current_fee_mean <- fee_mean(
          service_fees = plot_service_table()$service_fees,
          fee_names = plot_service_table()$fee_names,
          service_proportion_bulk = plot_service_table()$service_proportion_bulk,
          service_proportion_private = plot_service_table()$service_proportion_private,
          gap_fee = new_gap_fee,
          # set concessional bulk-billed proportion to 'y'
          # divide by 100 to convert from
          # percentage to proportion
          concessional_bulkbilled = y/100,
          individual_bulkbill_incentive,
          individual_bulkbill_incentive_by_fee = plot_service_table()$incentive_by_fee,
          input$monash
        )
        benefit_loss <- add_row(
          benefit_loss, gap = x,
          concessional_bulkbilled = y,
          current_fee_mean = current_fee_mean,
          benefit = benefit,
          benefit_rel = benefit / current_fee_mean * 100
        )
      }
    }
    benefit_loss
  }) |>
    bindEvent(
      plot_service_table(),
      input$monash, input$table_resolution
    )

  output$ggplot_benefitloss <- renderPlotly({

    limit <- max(abs(benefit_loss()$benefit)) * c(-1, 1)
    # set limit to help colour-scale centre to zero
    p <- ggplot(
      data = benefit_loss() |>
        dplyr::filter(
          gap == (gap %/% 5 * 5),
          concessional_bulkbilled == (concessional_bulkbilled %/% 5 * 5)
        ),
      aes(
        x = gap,
        y = concessional_bulkbilled,
        fill = benefit,
        text = sprintf(
          paste(
            "Gap: $%d<br>Concessional bulk-billed: %d%%<br>Current fee mean: $%.2f<br>",
            "Benefit: $%.2f<br>Revenue change (%%): %.1f%%", sep = ""
          ),
          gap, concessional_bulkbilled, current_fee_mean, benefit, benefit_rel
        )
      )) +
      geom_raster() +
      geom_text(aes(label=round(benefit, 2)), size = 8/.pt) +
      labs(
        x = "Mean Gap fee ($)",
        y = "Concessional bulk-billed (%)",
        fill = "Benefit ($)"
      ) +
      scale_x_continuous(breaks = seq(from = 0, to = 70, by = 10)) +
      scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10)) +
      ggtitle("Net benefit to practice, per service") +
      scale_fill_distiller(
        type = "div",
        palette = "Spectral",
        limit = limit
      ) +
      theme_tufte()

    ggplotly(p, tooltip = "text")
  })

  output$ggplot_benefitrelative <- renderPlotly({

    limit <- max(abs(benefit_loss()$benefit_rel)) * c(-1, 1)
    # set limit to help colour-scale centre to zero
    p <- ggplot(
      data = benefit_loss() |>
        dplyr::filter(
          gap == (gap %/% 5 * 5),
          concessional_bulkbilled == (concessional_bulkbilled %/% 5 * 5)
        ),
      aes(
        x = gap,
        y = concessional_bulkbilled,
        fill = benefit_rel,
        text = sprintf(
          paste(
            "Gap: $%d<br>Concessional bulk-billed: %d%%<br>Current fee mean: $%.2f<br>",
            "Benefit: $%.2f<br>Revenue change (%%): %.1f%%", sep = ""
          ),
          gap, concessional_bulkbilled, current_fee_mean, benefit, benefit_rel
        )
      )) +
      geom_raster() +
      geom_text(aes(label=round(benefit_rel, 1)), size = 8/.pt) +
      labs(
        x = "Mean Gap fee ($)",
        y = "Concessional bulk-billed (%)",
        fill = "Change in revenue (%)"
      ) +
      scale_x_continuous(breaks = seq(from = 0, to = 70, by = 10)) +
      scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10)) +
      ggtitle("Change in revenue (%)") +
      scale_fill_distiller(
        type = "div",
        palette = "Spectral",
        limit = limit
      ) +
      theme_tufte()

    ggplotly(p, tooltip = "text")
  })

  output$plotly_3d <- renderPlotly({

    if (input$zaxis_plotly3d == "Benefit ($)") {
      # plot benefit ($) on z-axis
      title_text <- "Benefit-loss according to mean gap fee and proportion concessional bulk-billed"
      z_variable <- as.formula("~benefit")
      zaxis_text <- "Net benefit<br>per service ($)"
      extra_text <- as.formula("~benefit_rel")
      hover_template <- paste(
        "Mean Gap fee: <b>$%{x}</b><br>",
        "Concessional bulk-billed: <b>%{y}</b>%<br>",
        "Current mean fee: <b>%{customdata:$.2f}</b><br>",
        "Benefit: <b>%{z:$.2f}</b><br>",
        "Revenue change (%): <b>%{text:.1f}</b>%",
        "<extra></extra>"
      )
    } else if (input$zaxis_plotly3d == "Revenue change (%)") {
      # plot change in revenue (%) on z-axis
      title_text <- "Revenue change according to mean gap fee and proportion concessional bulk-billed"
      z_variable <- as.formula("~benefit_rel")
      zaxis_text <- "Revenue change (%)"
      extra_text <- as.formula("~benefit")
      hover_template <- paste(
        "Mean Gap fee: <b>$%{x}</b><br>",
        "Concessional bulk-billed: <b>%{y}</b>%<br>",
        "Current mean fee: <b>%{customdata:$.2f}</b><br>",
        "Benefit: <b>%{text:$.2f}</b><br>",
        "Revenue change (%): <b>%{z:.1f}</b>%",
        "<extra></extra>"
      )
    }

    fig_plotly_3d <- plot_ly(
      data = benefit_loss(),
      x = ~gap, y = ~concessional_bulkbilled, z = z_variable,
      text = extra_text, customdata = ~current_fee_mean,
      type = "scatter3d", mode = "markers",
      hovertemplate = hover_template,
      marker = list(
        color = ~benefit,
        colorscale = "Rainbow",
      showscale = TRUE,
      line = list(width = 1, color = "DarkSlateGrey"))
    ) |>
      layout(
        title = title_text,
        scene = list(
          xaxis = list(title = list(text = "Mean Gap fee ($)")),
          yaxis = list(
            title = list(text = "Concessional<br>bulk-billed (%)"),
            autorange = "reversed"
          ),
          zaxis = list(title = list(text = zaxis_text)),
          # place viewpoint a little further away
          camera = list(eye = list(x = 1.75, y = 1.75, z = 1.25)),
          aspectmode = "cube"
        )
      )
  })

  mean_fee = reactive({
    v <- trimmed_service_table() |>
      # if there are empty values in service_proportion_raw_* then replace with 0
      replace_na(
        list(
          service_proportion_raw_bulk = 0,
          service_proportion_raw_private = 0
        )
      ) |>
      # recalculate service proportions
      mutate(
        service_proportion_bulk = service_proportion_raw_bulk/sum(service_proportion_raw_bulk),
        service_proportion_private = service_proportion_raw_private/sum(service_proportion_raw_private)
      )

    fee_mean(
      v$service_fees,
      v$fee_names,
      v$service_proportion_bulk,
      v$service_proportion_private,
      v$gap_fee,
      concessional_bulkbilled(),
      individual_bulkbill_incentive,
      v$incentive_by_fee,
      input$monash
    )
  }) |>
    bindEvent(
      trimmed_service_table(),
      input$monash,
      concessional_bulkbilled()
    )

  simple_benefit = reactive({
    v <- trimmed_service_table()
    net_benefit(
      v$service_fees,
      v$fee_names,
      v$service_proportion_bulk,
      v$service_proportion_private,
      v$gap_fee,
      concessional_bulkbilled(),
      individual_bulkbill_incentive,
      v$incentive_by_fee,
      input$monash,
      universal_bulkbill_incentive
    )
  }) |>
    bindEvent(
      trimmed_service_table(),
      input$monash,
      concessional_bulkbilled()
    )

  output$mean_fee_valuebox <- renderUI({
    bslib::value_box(
      title = "Calculated current mean fee",
      value = sprintf("$%.2f", mean_fee()),
      showcase = bs_icon("currency-dollar"),
      theme = "purple"
    )
  })
  output$mean_fee <- renderText({
    sprintf(
      "Calculated current Mean fee: $%.2f",
      "$%.2f",
      mean_fee()
    )
  })
  output$simple_benefit_valuebox <- renderUI({
    bslib::value_box(
      title = "Calculated Net Benefit",
      value = sprintf("$%.2f", simple_benefit()),
      showcase = bs_icon("graph-up-arrow"),
      theme = "teal"
    )
  })
  output$simple_benefit <- renderText({
    sprintf(
      "Calculated Net benefit: $%.2f",
      simple_benefit()
    )
  })
  output$simple_benefit_relative_valuebox <- renderUI({
    bslib::value_box(
      title = "Revenue change",
      value = sprintf("%.1f %%", simple_benefit()/mean_fee()*100),
      showcase = bs_icon("percent"),
      theme = "pink"
    )
  })
  output$simple_benefit_relative <- renderText({
    sprintf(
      "Revenue change: %.1f %%",
      simple_benefit()/mean_fee()*100
    )
  })

  observe({
    req(values[["build_mbs_table"]] == TRUE)
    insertUI(
      selector = '#mbs_table_placeholder',
      ## wrap element in a div with id for ease of removal
      ui = div(
        rHandsontableOutput(paste0("mbs_table_", mbs_table_version))
      )
    )
    output[[paste0("mbs_table_", mbs_table_version)]] = renderRHandsontable({
      DT <- NULL
      if (!is.null(input[[paste0("mbs_table_", mbs_table_version)]])) {
        DT <- hot_to_r(input[[paste0("mbs_table_", mbs_table_version)]]) |>
          # if there are empty values in service_proportion_raw_* then replace with 0
          # if a new row is created, it may contain lots of invalid NA values
          replace_na(
            list(
              service_proportion_raw_bulk = 0,
              service_proportion_raw_private = 0
            )
          ) |>
          # recalculate service proportions
          mutate(
            service_proportion_bulk = service_proportion_raw_bulk/sum(service_proportion_raw_bulk),
            service_proportion_private = service_proportion_raw_private/sum(service_proportion_raw_private)
          )
        if (input$all_gapfees_confirm) {
          # set all gap fees to the same value,
          # but if service item is set to 'fix_gap' then don't adjust gap fee
          DT <- DT |>
            mutate(
              gap_fee = if_else(
                fix_gap,
                gap_fee,
                input$all_gapfees
              )
            )
        }
        if (input$fix_all_gap_fees_confirm) {
          # set all
          DT <- DT |>
            mutate(fix_gap = as.logical(input$fix_all_gap_fees))
        }
        values[["service_table"]] <- DT

      } else if (!is.null(values[["service_table"]])) {
        DT <- values[["service_table"]]
      }

      if (!is.null(DT))
        rhandsontable(
          DT,
          stretchH = "all"
        ) |>
        hot_context_menu(
          # cannot change columns
          # (but rows can be added/removed)
          allowColEdit = FALSE
        ) |>
        hot_col(
          col = "incentive_by_fee",
          # only allow two choices
          type = "dropdown", source = c("single", "triple")
        ) |>
        hot_col(
          col = c("service_proportion_bulk", "service_proportion_private"),
          format = "0.00%",
          readOnly = TRUE,
        ) |>
        hot_validate_numeric(
          col = c(
            "service_fees",
            "service_proportion_raw_bulk", "service_proportion_raw_private"
          ),
          # cannot be negative numbers
          min = 0
        )
    })
    # need to enable when hidden, as the table is on a sub-panel
    outputOptions(output, paste0("mbs_table_", mbs_table_version), suspendWhenHidden = FALSE)
    # remove build trigger
    values[["build_mbs_table"]] <- FALSE
  }) |>
    bindEvent(values[["build_mbs_table"]])

}

# Run the application
shinyApp(ui = ui, server = server)
