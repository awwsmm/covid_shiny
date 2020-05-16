# get population density-normalized cumulative COVID deaths data
source("../functions/data.R", chdir=TRUE)

library(shinycssloaders) # for "loading" spinner on plot

source("../modules/dataControls.R", chdir=TRUE)
source("../modules/plotControls.R", chdir=TRUE)

testPageUI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "testPage", 
    fluidRow(
      column(width = 12,
        box(width = "100%", collapsible = TRUE, collapsed = FALSE,
          fluidRow(
            column(width = 12, withSpinner(type = getOption("spinner.type", default = 7),
              (plotlyOutput(ns("plot"), height="80vh")))))
    ) ) ),
    
    plotControlsUI(ns("plotControls")),
    dataControlsUI(ns("dataControls"))
  )
}

testPage <- function(input, output, session) {
  ns <- session$ns
  
  # get the reactiveVal(data.frame) from the data controls module
  data_reactive <- callModule(dataControls, "dataControls")
  
  # extract the returned values from the dataControls module
  df_reactive            <- reactive(data_reactive$df)
  statistic_reactive     <- reactive(data_reactive$statistic)
  normalization_reactive <- reactive(data_reactive$normalization)
  
  # get the plot object from the data controls module
  plot_reactive <- callModule(plotControls, "plotControls",
    df_reactive, statistic_reactive, normalization_reactive)

  # render the plot
  output$plot <- renderPlotly(plot_reactive())
}