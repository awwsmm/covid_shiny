source("data.R")

df <- as.data.frame(df)
g <- list(
  scope = 'world',
  projection = list(type = 'orthographic'),
  lakecolor = toRGB('white'))

mapPageUI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "mapPage", 
    fluidRow(
      column(width = 12,
        #h2("Work in Progress :: Map Page"),
        plotlyOutput(ns("plot"), height="50vh"),
      )
    ),
    sidebarPanel(
      sliderInput(ns("day"), "Days since initial death(s)",
                  min = 1, max = ncol(df),
                  value = 3,step=1)
      )
  )
}

mapPage <- function(input, output, session){
  ns <- session$ns
  
  d_reactive <- reactive({
    name <- colnames(df[input$day])
    day <- melt(df[name])
    return (day)
  }) 
 observe({
   d_obs <- d_reactive()
  plot <- plot_geo() %>%
          add_trace(z=d_obs$value,locations=rownames(df),locationmode="country names") %>%
          layout(geo=g)
  output$plot <- renderPlotly(plot)
 })
  }
