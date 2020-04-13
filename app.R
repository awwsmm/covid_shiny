library(shiny)
library(shinyWidgets)
library(plotly)
library(shinydashboard)

# get population density-normalized cumulative COVID deaths data
source("data.R")
df_orig <- getData()

# source all of the pages
source("dayPage.R")
source("mapPage.R")

ui <- dashboardPage(title = "COVID Data Explorer", skin = "black",

  header = dashboardHeader(title = "COVID Data Explorer"),

  sidebar = dashboardSidebar(
    menuItem("Daily Plot", tabName = "dayPage", icon = icon("dashboard")),
    menuItem("Map", tabName = "mapPage", icon = icon("th"))
  ),

  body = dashboardBody(
    tabItems(
      tabItem(tabName = "dayPage", dayPageUI("dayPageId", df_orig)),
      tabItem(tabName = "mapPage", mapPageUI("mapPageId"))
    )
  )
)

server <- function(input, output) {
  callModule(dayPage, "dayPageId", df_orig)
  callModule(mapPage, "mapPageId")
}

shinyApp(ui = ui, server = server)