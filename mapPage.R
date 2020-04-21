source("data.R")

df <- as.data.frame(df)
g <- list(
  scope = 'world',
  projection = list(type = 'kavrayskiy7'),
  lakecolor = toRGB('white'))

mapPageUI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "mapPage", 
    fluidRow(
      column(width = 12,
        #h2("Work in Progress :: Map Page"),
        plotlyOutput(ns("plot")),
      )
    ),
    sidebarPanel(
      sliderInput(ns("day"), "Days since initial death(s)",
                  min = 1, max = ncol(df),
                  value = 1,step=1,
                  animate=animationOptions(interval = 1, loop = TRUE))
      )
  )
}

mapPage <- function(input, output, session){
  ns <- session$ns
  plot_title <- HTML(paste0("Cumulative deaths per capita/km<sup>2</sup>"))
  d_reactive <- reactive({
    name <- colnames(df[input$day])
    day <- melt(df[name],id.vars=NULL)
    return (day)
  }) 
 observe({
  d_obs <- d_reactive()
  plot <- plot_geo() %>%
          add_trace(z=d_obs$value,locations=rownames(df),locationmode="country names") %>%
          layout(geo=g,title=plot_title)
  output$plot <- renderPlotly(plot)
 })
  }
