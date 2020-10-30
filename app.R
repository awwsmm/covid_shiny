library(shiny)
library(shinyWidgets)
library(plotly)
library(shinydashboard)
library(shinycssloaders) # for "loading" spinners

# get population density-normalized cumulative COVID deaths data
source("functions/data.R", chdir=TRUE)

# load the UI modules
source("modules/dataControls.R", chdir=TRUE)
source("modules/plotControls.R", chdir=TRUE)

# source all of the pages
source("pages/dayPage.R", chdir=TRUE)
source("pages/bhatiaPage.R", chdir=TRUE)
source("pages/mapPage.R", chdir=TRUE)
source("pages/tablePage.R", chdir=TRUE)

ui <- dashboardPage(title = "COVID Data Explorer", skin = "black",

  header = dashboardHeader(title = "COVID Data Explorer"),

  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Data vs. Time", tabName = "dayPage", icon = icon("chart-line")),
#     menuItem("Bhatia Plot", tabName = "bhatiaPage", icon = icon("chart-line")),
#     menuItem("Map", tabName = "mapPage", icon = icon("map-marked-alt")),
      menuItem("Data Table", tabName = "tablePage", icon = icon("table"))
    ),
    p(style="padding: 20px", HTML(paste0(
      "Built by <a href='https://github.com/templardrake'>templardrake</a> ",
      "and <a href='https://github.com/awwsmm'>awwsmm</a>, with data from ",
      "<a href='https://github.com/CSSEGISandData'>Johns Hopkins University</a> ",
      "(via <a href='https://github.com/pomber'>pomber</a>) and ",
      "<a href='https://github.com/samayo'>samayo</a>."))
    ),
    p(style="text-align: center", HTML(paste0(
      "<a href='https://github.com/awwsmm/covid_shiny'>",
      "<img src='images/GitHub-Mark-Light-32px.png' /></a>"
    )))
  ),

  body = dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tabItems(
      dayPageUI("dayPageId"),
#     bhatiaPageUI("bhatiaPageId"),
#     mapPageUI("mapPageId"),
      tablePageUI("tablePageId")
    )
  )
)

server <- function(input, output) {
  callModule(dayPage, "dayPageId")
# callModule(bhatiaPage, "bhatiaPageId")
# callModule(mapPage, "mapPageId")
  callModule(tablePage, "tablePageId")
}

shinyApp(ui = ui, server = server)