source("data.R")

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
      sliderInput("integer", "Days since initial death(s)",
                  min = 1, max = ncol(df),
                  value = 1,step=1)
      )
  )
}

mapPage <- function(input, output, session){
  ns <- session$ns

 last <- tail(colnames(df),n=1)
 
 last_df <- melt(df[,last])   
  #print(head(last_df))
  plot <- plot_geo() %>% 
          add_trace(z= last_df$value,
                            span = I(0), 
                            locations = rownames(last_df), 
                            locationmode="country names") %>% 
                            layout(geo = g)
  
  output$plot <- renderPlotly(plot)
  }
