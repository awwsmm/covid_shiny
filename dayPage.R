# get population density-normalized cumulative COVID deaths data
source("data.R")
library(nls2)    # for nonlinear curve fitting (logistic growth)
library(memoise) # for memoization of computationally-intensive functions

# function to turn a string term into a formula
tilde <- function(term) as.formula(paste0("~`", term, "`"))

# function to add a country to the plot
add_country <- function(country, plot, mode)
  plot %>% add_trace(y = tilde(country), name = country, mode = mode)

# attempts to fit a bi-logistic to the given x and y values (else a logistic) or returns NULL
fit <- memoise(function(df) {
  
  # only attempt a fit if there are at least 10 points
  if (nrow(df) < 10) return(NULL)

  # estimate midpoint of exponential rise
  y_mid <- (max(df$y) - min(df$y)) / 2.0
  x_mid <- df[which(df$y > y_mid)[1], "x"]
  
  # initial fit parameters and model configuration
  start1 <- list(L1 = max(df$y)*0.9, k1 = 0.16, x1 = x_mid)
  control1 <- list(maxiter = 200, tol = 1e-08, minFactor = 1/(2^16), warnOnly=TRUE)
  
  tryCatch({ # naive growth model: logistic
    func1 <- y ~ L1 / (1+exp(-k1*(x-x1)))
    model1 <- nls2(func1, df, start = start1, control = control1, weights = ~0.3*(max(x)-x))
  
    # refined growth model: bi-logistic
    start2 <- c(model1$m$getPars(), L2 = max(df$y)*0.1, k2 = 0.3, x2 = max(df$x))
    control2 <- list(maxiter = 200, tol = 1e-08, minFactor = 1/(2^16), warnOnly=TRUE)
    func2 <- y ~ L1 / (1+exp(-k1*(x-x1))) + L2 / (1+exp(-k2*(x-x2)))
    
    # bound the parameters of the larger logistic function
    minima <- 0.7*start2
    maxima <- 1.3*start2
  
    # try bi-logistic, default to logistic
    tryCatch(
      nls(func2, df, algorithm = "port", start = start2,
        control = control2, lower = minima, upper = maxima),
      error   = function(e) model1,
      warning = function(e) model1 )

  # if not even the logistic model fits, return NULL
  }, error = function(e) NULL )

})

# function to add a Logistic growth curve fit
add_fit <- function(df, x, y, plot, extrapolate) {
  if (is.null(extrapolate)) extrapolate <- FALSE
  
  # get only the x and y variables, remove incomplete rows
  df <- df[ , c(x, y)]
  df <- df[complete.cases(df), ]
  
  # convert x-values from POSIXct to numeric
  df$x <- as.numeric(df[ , x])
  df$y <- df[ , y]
  
  # get the fit model, if not NULL, draw the line on the plot
  model <- fit(df)
  if (!is.null(model)) {

    # get first and last day, and add 4 weeks to end for extrapolation
    x_extended <- min(df$x):(max(df$x)+extrapolate*28)
    t_extended <- if (min(df$x) < 10000) x_extended
      else as.Date(x_extended, origin = "1970-01-01")
    
    plot <- add_lines(plot,
      x = t_extended,
      y = predict(model, data.frame(x = x_extended)),
      name = paste0("Fit for ", y),
      line = list(dash = "dash"))
    
  # if model fails, return plot without line added
  } else {
    print(paste0("Failed to fit: ", y))
    plot
  }
}

dayPageUI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "dayPage", 
    fluidRow(
      column(width = 12,
        box(width = "100%", collapsible = TRUE, collapsed = FALSE,
          fluidRow(
            column(width = 12, plotlyOutput(ns("plot"), height="80vh"))
          ),
          fluidRow(
            column(width = 4,
              materialSwitch(ns("log_y"), status = "success",
                HTML("<span class='matswitchlabel'>Logarithmic</span>"))
            ),
            column(width = 4,
              materialSwitch(ns("show_markers"), status = "success",
                HTML("<span class='matswitchlabel'>Show Markers</span>"))
            ),
            column(width = 4,
              materialSwitch(ns("show_legend"), status = "success", value = TRUE,
                HTML("<span class='matswitchlabel'>Show Legend</span>"))
            )
          )
    ) ) ),
    
    fluidRow(
      column(width = 12,
        box(title = "Plot Controls", status = "info", width = NULL,
          solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
          
          fluidRow(
            column(width = 4,
              uiOutput(ns("user_countries_UI")),
              
              selectInput(ns("statistics"), "Data to plot",
                choices = statistics, selected = "deaths" ),
              
              selectInput(ns("normalizations"), "Normalize by",
                choices = normalizations, selected = "population" )
            ),
            column(width = 4,
              
              selectInput(ns("plot_against"), "Plot against",
                choices = c("Date", "Days Since..."), selected = "Date"),
              
              uiOutput(ns("days_since_UI")),
              
              uiOutput(ns("data_selection_error"))
            ),
            column(width = 4,
              uiOutput(ns("plot_line_UI")),
#              materialSwitch(ns("show_errors"), status="success",
#                HTML("<span class='matswitchlabel'>Show Errors</span>")),
              uiOutput(ns("show_forecast_UI"))
            )
          )
    ) ) )
  )
}

dayPage <- function(input, output, session) {
  ns <- session$ns
  
  # listen to x-axis alignment selection and render this separately from other UI
  observe({
    if (input$plot_against == "Days Since...") {

      # set the number of ticks on the slider
      n_ticks <- 8

      # try to get equal numbers of points in each vertical slice
      unique_values <- uniqueValues(input$statistics, input$normalizations)
      indices <- round(length(unique_values) / (n_ticks+1)) * 1:n_ticks
      choices <- unique_values[indices]
      
      # dynamically create slider input label
      label <- "...cumulative"
      if (input$normalizations != "none") label <- paste(label, "normalized")
      label <- paste(label, input$statistics)
      
      output$days_since_UI <- renderUI(
        sliderTextInput(ns("days_since"), tolower(label),
          choices=formatC(choices, format="g", digits=2), grid=T))
      
    } else output$days_since_UI <- NULL
  })
  
  # reactive data frame
  df_orig_reactive <- reactiveVal(NULL)
  
  # save previously-selected countries so they can be reselected
  old_countries <- reactiveVal(c("Ireland", "US", "Italy", "United Kingdom", "Spain", "France", "Germany", "Japan"))
  
  # ignoreInit so old_countries isn't set to NULL on initialization
  observeEvent({ input$statistics; input$normalizations }, {
    old_countries(input$user_countries)
  }, ignoreInit = TRUE)
  
  # ...update when statistic or normalization selection changes
  observe({
    current_stat <- if (is.null(input$statistics))         statistics[1] else input$statistics
    current_norm <- if (is.null(input$normalizations)) normalizations[1] else input$normalizations
    
    df_orig <- getData(current_stat, current_norm)
    df_orig_reactive(df_orig)
    
    output$user_countries_UI <- renderUI(
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
        selected = intersect(rownames(df_orig), old_countries()),
        width = "100%",
      )
    )
  })
  
  # save previously-selected "Plot Line" choice so it can be reselected
  old_plot_line <- reactiveVal("Simple Linear Interpolation")
  
  # ignoreInit so old_plot_line isn't set to NULL on initialization
  observeEvent(input$show_markers, {
    old_plot_line(input$plot_line)
  }, ignoreInit = TRUE)
  
  # render "plot line" UI, changes when "show_markers" changes
  observe({
    
    choices <- c("Simple Linear Interpolation", "Best-Fit (Bi-)Logistic Curve")
    if (input$show_markers) choices <- c(choices, "None")
    
    output$plot_line_UI <- renderUI(
      selectInput(ns("plot_line"), "Plot Line",
        choices = choices,
        selected = old_plot_line()
      )
    )
  })
  
  # render "show forecast" UI, changes when "plot_line" changes
  observeEvent(input$plot_line, {
    
    output$show_forecast_UI <-
      if (input$plot_line == "Best-Fit (Bi-)Logistic Curve")
        renderUI(materialSwitch(ns("show_forecast"), status="success",
          HTML("<span class='matswitchlabel'>Show Forecast</span>")))
      else NULL
    
    # markers are only optional with a simple linear interpolation
    if (input$plot_line == "Simple Linear Interpolation") {
      shinyjs::show("show_markers")
    } else {
      updateMaterialSwitch(session, ("show_markers"), value = TRUE)
      shinyjs::hide("show_markers")
    }
    
    
  }, ignoreInit = TRUE)
  
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
      if (input$plot_against == "Date") {
        
        # what to plot along the x-axis
        xval <- "Date"
        
        # transpose and add a Date column
        tf <- as.data.frame(t(df))
        tf$Date <- as.Date(rownames(tf))
        
        # countries to use? all of them
        countries <- input$user_countries
        
        # complex row-wise shifting of data
      } else if (input$plot_against == "Days Since...") {
        
        # what to plot along the x-axis
        xval <- "Days Since..."
        minrate <- if (is.null(input$days_since)) 0.1 else input$days_since
        
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
        
        # show just lines? or markers and lines? or curve fit?
        mode <- if (input$plot_line == "Best-Fit (Bi-)Logistic Curve" || input$plot_line == "None") "markers"
                else if (input$show_markers) "lines+markers"
                else "lines"
        
        # create plot with first country
        first_country <- countries[1]
        plot <- plot_ly(tf, x=tilde(xval), y=tilde(first_country), name=first_country,
          type="scatter", mode=mode)
        
        # add all additional countries in a loop
        if (len >1) for (ii in 2:len) plot <- add_country(countries[ii], plot, mode)
        
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
        if (input$log_y) yaxis <- c(yaxis, type="log")
        
        plot <- plot %>% layout(
          title = plot_title,
          xaxis = list(title = xval),
          yaxis = yaxis,
          margin = list(l = 50, r = 50, b = 80, t = 80, pad = 20),
          showlegend = input$show_legend
        )
        
        # if "Best-Fit Logistic Curve", add curve fits
        if (input$plot_line == "Best-Fit (Bi-)Logistic Curve")
          for (ii in 1:len) plot <- add_fit(tf, xval, countries[ii], plot, input$show_forecast)
        
        output$plot <- renderPlotly(plot)
        
      } else {
        output$data_selection_error <- renderUI(
          valueBox("Error", "No data to plot!", icon=icon("exclamation-circle"), color="red", width=12)
        )
      }
    }
  })
}