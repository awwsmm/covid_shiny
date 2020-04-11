library(shiny)
library(shinyWidgets)


source("data.R")

# get population density-normalized cumulative COVID deaths data
df <- getData()

# UI
# Makes line plot over time using user input

ui <- fluidPage(
  title = paste("Cumulative deaths over time by country"),
  fluidRow(
    column(width = 12,
           plotlyOutput("deathPlot"),
           pickerInput(
             inputId = "userCountries", label = "Countries :",
             choices = rownames(df),
             options = list(`actions-box` = TRUE),multiple = T,
             selected = "US", width = "350px"
           )
    )
  )
  
)


#Shiny server plot


server <- function(input, output) {
  tf <- as.data.frame(t(df))
  tf$Date <- as.Date(rownames(tf))

  plotly_country <- function(country,deaths) {
    deaths %>% add_trace(y = as.formula(paste0("~`", country, "`")), 
    name = country, mode = 'lines') 
    }
  x <- list(
    title = "Date"
  )
  y <- list(
    title = "Deaths"
  )

observe({  
  
len <- length(input$userCountries)

if(len > 0){
    firstCountry <- input$userCountries[1]
      deaths <- plot_ly(tf, x = ~Date, y = as.formula(paste0("~`", firstCountry, "`")),name=firstCountry,type = 'scatter', mode = 'lines')
      if(len >1)
        for(i in 2:len)
          deaths <- plotly_country(input$userCountries[i],deaths)
      deaths <- deaths %>% layout(title="Number of deaths over time",xaxis = x, yaxis = y)
      output$deathPlot <- renderPlotly(deaths)    
      }
    }
  )
}

shinyApp(ui = ui, server = server)