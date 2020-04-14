library(shiny)
library(shinyWidgets)
library(plotly)
library(shinydashboard)

# source all of the pages
source("dayPage.R")
source("mapPage.R")

ui <- dashboardPage(title = "COVID Data Explorer", skin = "black",

  header = dashboardHeader(title = "COVID Data Explorer"),

  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Data vs. Time", tabName = "dayPage", icon = icon("dashboard")),
      menuItem("Map", tabName = "mapPage", icon = icon("th"))
    ),
    p(style="padding: 20px", HTML(paste0(
      "Built by <a href='https://github.com/templardrake'>templardrake</a> ",
      "and <a href='https://github.com/awwsmm'>awwsmm</a>, with data from ",
      "<a href='https://github.com/CSSEGISandData'>Johns Hopkins University</a> ",
      "(via <a href='https://github.com/pomber'>pomber</a>) and ",
      "<a href='https://github.com/samayo'>samayo</a>."))
    )
  ),

  body = dashboardBody(
    tabItems(
      dayPageUI("dayPageId"),
      mapPageUI("mapPageId")
    )
  )
)

server <- function(input, output) {
  callModule(dayPage, "dayPageId")
  callModule(mapPage, "mapPageId")
}

shinyApp(ui = ui, server = server)