# Shiny video tutorial available at: https://shiny.rstudio.com/tutorial
# Written tutorial at https://shiny.rstudio.com/tutorial/written-tutorial/lesson1

# simplest possible Shiny app -- no inputs or graphical outputs

library(shiny)

# UI defines graphical interface which will appear in web browser
# check out shinydashboard for different kinds of UIs
ui <- fluidPage()

# server defines backend logic, must be executed on a server with an R installation
# Shiny apps can be hosted on your own server, or Shiny hosting websites like shinyapps.io
server <- function(input, output) {
  print("Hello, World!")
}

shinyApp(ui = ui, server = server)