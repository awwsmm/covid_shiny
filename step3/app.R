# simple Shiny app with one input and one output
#   inputs are more complex: reactive(), observe(), etc.

library(shiny)
library(plotly)
library(ggplot2)

# check out different outputs and inputs on this Shiny cheatsheet
#   https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
ui <- fluidPage(
  fluidRow(
    column(width = 6,
      numericInput("nObs", "number of observations", 50, 1, 1000, 1)
    ),
    column(width = 6,
      plotlyOutput("aNewPlot")
    )
  )
)

server <- function(input, output) {
  
  # create a simple histogram (this throws an error)...
#  simpleHist <- hist(rnorm(input$nObs))
  
  # ...try this instead
  simpleHist <- plot_ly(x = ~rnorm(input$nObs), type = "histogram")
  
  # ...and pipe it to the UI
  output$aNewPlot <- renderPlotly(simpleHist)
  
  # We could also use ggplot -- more complicated
  # protoHist <- reactive({
  #   df <- as.data.frame(rnorm(input$nObs))
  #   names(df) <- "rand"
  #   ggplot(df, aes(rand)) + geom_histogram()
  # })
  # 
  # check out this tutorial on reactive sinks, conductors, and endpoints
  #   https://shiny.rstudio.com/articles/reactivity-overview.html
  # 
  # ...also, will give a "Cairo" error on macOS
  # output$aNewPlot <- renderPlotly(ggplotly(protoHist()))
}

shinyApp(ui = ui, server = server)