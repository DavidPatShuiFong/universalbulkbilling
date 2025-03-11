#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tibble)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rhandsontable)
library(htmlwidgets)

service_items <- read.csv("./medicarebenefits.csv") |>
  # calculate the proportions
  # for simplicity, assume the same proportions for bulk-billed patients
  # and privately-billed patients
  mutate(
    service_proportion_raw_bulk = service_proportion_raw,
    service_proportion_raw_private = service_proportion_raw
  ) |>
  mutate(
    service_proportion_bulk = service_proportion_raw_bulk/sum(service_proportion_raw_bulk),
    service_proportion_private = service_proportion_raw_private/sum(service_proportion_raw_private)
  ) |>
  select(
    fee_names, service_fees, incentive_by_fee = individual_bulkbill_incentive_by_fee,
    gap_fee, privately_billed,
    service_proportion_bulk, service_proportion_private,
    service_proportion_raw_bulk, service_proportion_raw_private
  )

# the individual service bulk-bill incentive (dollars)
# varies by location
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

# universal bulk billing incentive
# over the service fee
# set to 12.5%
universal_bulkbill_incentive <- 0.125

fee_mean <- function(
  # calculate the current mean fee
  service_fees,
  fee_names,
  service_proportion_bulk,
  service_proportion_private,
  gap_fee,
  privately_billed,
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

  sum(privately_billed * service_fees * service_proportion_private) +
    sum((1 - privately_billed) * service_fees * service_proportion_bulk) +
    # the calculation above could be simplified, of course!, to
    # sum(service_fees * service_proportion_private), but is kept
    # separate to two different portions to clarify the calculation

    # calculate (and add) the individual service bulk-billing incentive applied
    # to patients currently bulk-billed
    sum((1 - privately_billed) * service_proportion_private * fee_bulkbill_incentive) +

    # calculate (and add) the private 'gap' fee
    sum(privately_billed * service_proportion_private * gap_fee)
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
  privately_billed,
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
  sum(privately_billed * service_fees * service_proportion_private) *
    universal_bulkbill_incentive +
    # calculate (and add) benefit for patients already bulk-billed
    sum((1 - privately_billed) * service_fees * service_proportion_bulk) *
    universal_bulkbill_incentive +

    # calculate (and add) the individual service bulk-billing incentive applied
    # to patients formerly privately-billed
    sum(privately_billed * service_proportion_private * fee_bulkbill_incentive) -

    # calculate (and subtract) the loss from not charging a private 'gap' fee
    sum(privately_billed * service_proportion_private * gap_fee)
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Universal bulk-billing benefit-loss"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          selectInput(
            "monash",
            "Select Monash area",
            dimnames(individual_bulkbill_incentive)[[1]]
            ),
        ),
        # Show a plot of the generated distribution
        mainPanel(

          textOutput("mean_fee"),
          textOutput("benefit_loss"),
          textOutput("myoutput"),
          rHandsontableOutput("mbs_table"),

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    values = reactiveValues(
      service_table = service_items
    )

    mean_fee = reactive({
      browser()
      v <- values[["service_table"]] |>
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
      values[["service_table"]] = v

      v <- v |>
        # get rid of any rows with empty values
        filter(!is.na(fee_names) & !is.na(service_fees) &
                 !is.na(incentive_by_fee) & !is.na(gap_fee) &
                 !is.na(privately_billed) & !is.na(service_proportion_bulk) &
                 !is.na(service_proportion_private))

      fee_mean(
        v$service_fees,
        v$fee_names,
        v$service_proportion_bulk,
        v$service_proportion_private,
        v$gap_fee,
        v$privately_billed,
        individual_bulkbill_incentive,
        v$incentive_by_fee,
        input$monash
      )
    })

    benefit_loss = reactive({
      v <- values[["service_table"]] |>
        # get rid of any rows with empty values
        filter(!is.na(fee_names) & !is.na(service_fees) &
                 !is.na(incentive_by_fee) & !is.na(gap_fee) &
                 !is.na(privately_billed) & !is.na(service_proportion_bulk) &
                 !is.na(service_proportion_private))
      net_benefit(
        v$service_fees,
        v$fee_names,
        v$service_proportion_bulk,
        v$service_proportion_private,
        v$gap_fee,
        v$privately_billed,
        individual_bulkbill_incentive,
        v$incentive_by_fee,
        input$monash,
        universal_bulkbill_incentive
      )
    })

    adjust_proportions = reactive({
      browser()
      v <- values[["service_table"]]
      v <- values[["service_table"]] |>
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
      values[["service_table"]] = v
    })

    output$distPlot <- renderPlot({
      x <- faithful[, 2]  # Old Faithful Geyser data
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, breaks = bins, col = 'darkgray', border = 'white',
           xlab = 'Waiting time to next eruption (in mins)',
           main = 'Histogram of waiting times')
    })

    output$myoutput <- renderText({values[["service_table"]]$fee_names})

    output$mean_fee <- renderText({
      sprintf(
        "Mean fee: $%.2f",
        mean_fee()
      )
    })
    output$benefit_loss <- renderText({
      sprintf(
        "Net benefit: $%.2f",
        benefit_loss()
      )
    })

    output$mbs_table = renderRHandsontable({
      DT = NULL
      if (!is.null(input$mbs_table)) {
        DT = hot_to_r(input$mbs_table)
        # if a new row is created, it may contain lots of invalid NA values
        values[["service_table"]] = DT
      } else if (!is.null(values[["service_table"]])) {
        DT = values[["service_table"]]
      }

      if (!is.null(DT))
        rhandsontable(
          DT
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
            "service_fees", "privately_billed",
            "service_proportion_raw_bulk", "service_proportion_raw_private"
          ),
          # cannot be negative numbers
          min = 0
        ) |>
        hot_validate_numeric(
          col = c("privately_billed"),
          min = 0, max = 1
        )
    })

}

# Run the application
shinyApp(ui = ui, server = server)
