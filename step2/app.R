# Adapted from https://plotly.com/r/line-charts/

# simple Shiny app with no inputs, only outputs

library(shiny)
library(plotly)

# global data -- available within server and within ui
N <- 100

ui <- fluidPage(
  title = paste("My plot with", N, "points"),
  fluidRow(
    column(width = 12,
      plotlyOutput("myPlot")
    )
  )
)

server <- function(input, output) {
  
  x <- c(1:N)
  
  # some example data -- available only within server
  trace_0 <- rnorm(100, mean = 5)
  trace_1 <- rnorm(100, mean = 0)
  trace_2 <- rnorm(100, mean = -5)
  
  data <- data.frame(x, trace_0, trace_1, trace_2)
  
  fig <- plot_ly(data, x = ~x, y = ~trace_0, name = 'trace 0', type = 'scatter', mode = 'lines') 
  fig <- fig %>% add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers') 
  fig <- fig %>% add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers')
  
  # you can call renderPlotly() on a ggplot, as well, but it doesn't play nicely on macOS
  output$myPlot <- renderPlotly(fig)
}

shinyApp(ui = ui, server = server)