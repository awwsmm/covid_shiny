# get population density-normalized cumulative COVID deaths data
source("data.R")

# function to turn a string term into a formula
tilde <- function(term) as.formula(paste0("~`", term, "`"))

# function to add a trace to a Plotly plot
add_country <- function(country, plot)
  plot %>% add_trace(y = tilde(country), name = country, mode = 'lines')

dayPageUI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "dayPage", 
    fluidRow(
      column(width = 12, box(width = "100%", plotlyOutput(ns("plot"), height = 500)))
    ),
    
    fluidRow(
      column(width = 12,
        box(
          title = "Plot Controls",
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          collapsed = FALSE,
          
          fluidRow(
            column(width = 4,
              selectInput(ns("statistics"), "Data to plot",
                choices = statistics,
                selected = "deaths"
              ),
              selectInput(ns("normalizations"), "Normalize by",
                choices = normalizations,
                selected = "population-density"
              ),
              materialSwitch(ns("logy"), HTML(
                "<span style='font-weight: bold; display: block; padding-bottom: 10px'>Logarithmic</span>"
                ), status="success")
            ),
            column(width = 4,
              uiOutput(ns("country_picker"))
            ),
            column(width = 4,
              selectInput(ns("alignx"), "Plot against",
                choices = c("Date", "Days Since..."),
                selected = "Date"
              ),
              uiOutput(ns("days_since")),
              uiOutput(ns("data_selection_error"))
            )
          )
        )
      )
    )
    
  )
}

dayPage <- function(input, output, session) {
  
  ns <- session$ns
  
  # listen to x-axis alignment selection and render this separately from other UI
  observe({
    if (input$alignx == "Days Since...")
      output$days_since <- renderUI(
        sliderTextInput(ns("xaxis_rate_align"), "...cumulative normalized deaths",
          choices=c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000), selected=0.0001, grid=T))
    else
      output$days_since <- NULL
  })
  
  # reactive data frame
  df_orig_reactive <- reactiveVal(NULL)
  
  # ...update when statistic or normalization selection changes
  observe({
    current_stat <- if (is.null(input$statistics))         statistics[1] else input$statistics
    current_norm <- if (is.null(input$normalizations)) normalizations[1] else input$normalizations
    
    df_orig <- getData(current_stat, current_norm)
    df_orig_reactive(df_orig)
    
    output$country_picker <- renderUI(
      pickerInput(ns("user_countries"), "Select countries",
        choices = sort(rownames(df_orig)),
        options = pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          liveSearchNormalize = TRUE,
          liveSearchPlaceholder = "Search",
          size = 10
        ),
        multiple = T,
        selected = c("Ireland", "US", "Italy", "United Kingdom", "Spain", "France", "Germany", "Japan"),
        width = "100%",
      )
    )
    
  })
  
  observe({
    
    # get the data as a non-reactive data frame
    df_orig <- df_orig_reactive()
    
    # create plot only if user has selected at least one country
    len <- length(input$user_countries)
    if (len > 0) {
      
      # copy original data, but only use user-selected countries
      df <- df_orig[input$user_countries, , drop=FALSE]
      countries <- NULL
      
      # transposed data with additional "Date" column
      if (input$alignx == "Date") {
        
        # what to plot along the x-axis
        xval <- "Date"
        
        # transpose and add a Date column
        tf <- as.data.frame(t(df))
        tf$Date <- as.Date(rownames(tf))
        
        # countries to use? all of them
        countries <- input$user_countries
        
        # complex row-wise shifting of data
      } else if (input$alignx == "Days Since...") {
        
        # what to plot along the x-axis
        xval <- "Days Since..."
        minrate <- if (is.null(input$xaxis_rate_align)) 0.1 else input$xaxis_rate_align
        
        # get day (index) where country first met or exceeded `minrate` density-normalized cumulative deaths
        # will be on range [1, n_days + 1]
        start <- as.vector(tail(apply(df < minrate, 1, cumsum), n = 1)) + 1
        
        # if number of days > `start`, country hasn't had that many cases yet
        n_days <- ncol(df)
        
        # these are all the countries with >= `minrate` cumulative deaths rate
        countries <- rownames(df)[start < n_days]
        
        if (length(countries) > 0) {
          
          # rewrite data frame and start date list
          df <- df[countries, , drop=FALSE]
          start <- start[start < n_days]
          names(start) <- countries
          
          # shift all rows to the left by `start` columns
          minoffset <- min(start)
          offset <- start-minoffset
          
          # This is the most complicated part -- we need to "shift" each row of the data frame.
          # If Japan has an offset of -49, we need to move every element of the "Japan" row
          # 49 columns to the right. If Luxembourg has an offset of 5, we need to move every
          # element of the "Luxembourg" row 5 columns to the left.
          
          shift <- function(df, row, by) {
            nr <- nrow(df)
            if (by == 0 || row > nr || row < 1) df
            else {
              nc <- ncol(df)
              if (abs(by) >= nc) {
                df[row, ] <- rep(NA, nc)
                df
              } else if (by > 0) {
                df[row, 1:(nc-by)] <- df[row, (1+by):nc]
                df[row, (nc-by+1):nc] <- NA
                df
              } else {
                df[row, (1-by):nc] <- df[row, 1:(nc+by)]
                df[row, 1:(-by)] <- NA
                df
              }
            }
          }
          
          # shift all rows by the appropriate amount
          for (ii in 1:length(countries)) df <- shift(df, ii, offset[ii])
          
          # finally, trim negative days, transpose and add "Days Since..." column, similar to above
          df <- df[ , minoffset:(ncol(df)), drop=FALSE]
          tf <- as.data.frame(t(df))
          tf$`Days Since...` <- 0:(nrow(tf)-1)
        }
      }
      
      # create plot only if at least one country is left after any filtering
      len <- length(countries)
      if (len > 0) {
        
        # clear the "data selection" error
        output$data_selection_error <- NULL
        
        # create plot with first country
        first_country <- countries[1]
        plot <- plot_ly(tf, x=tilde(xval), y=tilde(first_country), name=first_country, type="scatter", mode="lines")
        
        # add all additional countries in a loop
        if (len >1) for (ii in 2:len) plot <- add_country(countries[ii], plot)
        
        # create the plot title / y-axis title
        plot_title <- HTML(paste0("Cumulative ",

          if (input$statistics == "confirmed") "Confirmed Cases"
          else if (input$statistics == "deaths") "Deaths"
          else if (input$statistics == "recovered") "Recovered",
          
          if (input$normalizations == "none") ""
          else if (input$normalizations == "population") " per capita"
          else if (input$normalizations == "population-density") " per capita/km<sup>2</sup>"
          ))
        
        yaxis <- list(title=plot_title, tickprefix="   ")
        if (input$logy) yaxis <- c(yaxis, type="log")
        
        plot <- plot %>% layout(
          title = plot_title,
          xaxis = list(title = xval),
          yaxis = yaxis,
          margin = list(l = 50, r = 50, b = 80, t = 80, pad = 20),
          showlegend = TRUE
        )
        
        output$plot <- renderPlotly(plot)
        
      } else {
        output$data_selection_error <- renderUI(
          valueBox("Error", "No data to plot!", icon=icon("exclamation-circle"), color="red", width=12)
        )
      }
    }
  })
}