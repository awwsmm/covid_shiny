# get population density-normalized cumulative COVID deaths data
source("../functions/data.R", chdir=TRUE)
library(memoise)

windowed_data <- memoise(function(df, n_days) {
  ncols <- ncol(df)
  
  # create "lagging" dataframe from N days earlier
  lag <- df
  window_width <- min(max(1, n_days), ncols-1)
  
  for (ii in 1:window_width)
    lag <- cbind(0, lag[ , -ncols])
  
  # subtract lag to "unaccumulate"
  df - lag
})

bhatiaPageUI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "bhatiaPage", 
    fluidRow(
      column(width = 12,
        plotlyOutput(ns("plot"))
      )
    )
  )
}

# want to be able to
#  - select statistic and normalization
#  - show / hide overall best-fit line
#  - slider for time

bhatiaPage <- function(input, output, session) {
  
  # get data (cumulative) and un-accumulate it
  df_sum <- getData("confirmed", "population")
  df <- windowed_data(df_sum, 7)

  countries <- c("Ireland", "US", "Italy", "United Kingdom", "Spain", "France", "Germany", "Japan")
  
  # create plot with first country
  first_country <- countries[1]
  len <- length(countries)
  
  tmp_df <- log10(data.frame(df_sum[first_country, ], df[first_country, ]))
#  colnames(tmp_df) <- c("Cumulative Deaths as of [Date]", "Deaths in Preceding [7] Days")
  colnames(tmp_df) <- c("xx", "yy")
  
  plot <- plot_ly(tmp_df, x=~xx, y=~yy, name=first_country, type="scatter", mode="lines")
  
  # add all additional countries in a loop
  if (len > 1) for (ii in 2:len) {
    country <- countries[ii]
    tmp_df <- log10(data.frame(df_sum[country, ], df[country, ]))
#  colnames(tmp_df) <- c("Cumulative Deaths as of [Date]", "Deaths in Preceding [7] Days")
  colnames(tmp_df) <- c("xx", "yy")
    plot <- plot %>% add_lines(data = tmp_df, x=~xx, y=~yy, name = country, mode = "lines")
  }
  
  output$plot <- renderPlotly(plot)
  
}