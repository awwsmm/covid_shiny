library(shiny)

source("data.R")

# get population density-normalized cumulative COVID deaths data
df <- getData()


ui <- fluidPage(
  
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)