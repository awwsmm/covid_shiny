library(nls2)            # for nonlinear curve fitting (logistic growth)
library(memoise)         # for memoization of computationally-intensive functions

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

defaultPlotControlsUI <- function(
    logyToggleUI, markersToggleUI, legendToggleUI, plotAgainstUI,
    daysSinceUI, dataSelectionErrorUI, plotLineUI, errorsToggleUI, forecastToggleUI
  ) {
  box(title = "Plot Controls", status = "info", width = NULL,
    solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
    
    fluidRow(
      column(width = 4, logyToggleUI),
      column(width = 4, markersToggleUI),
      column(width = 4, legendToggleUI)
    ),
    fluidRow(
      column(width = 4,
        plotAgainstUI,
        daysSinceUI,
        dataSelectionErrorUI
      ),
      column(width = 4,
        plotLineUI,
#         errorsToggleUI,
        forecastToggleUI
      )
    )

  )
}

plotControlsUI <- function(id, layout = defaultPlotControlsUI) {
  ns <- NS(id)
  layout(
    uiOutput(ns("logyToggleUI")),
    uiOutput(ns("markersToggleUI")),
    uiOutput(ns("legendToggleUI")),
    uiOutput(ns("plotAgainstUI")),
    uiOutput(ns("daysSinceUI")),
    uiOutput(ns("dataSelectionErrorUI")),
    uiOutput(ns("plotLineUI")),
    uiOutput(ns("errorsToggleUI")),
    uiOutput(ns("forecastToggleUI"))
  )
}

# listen to the plot-control inputs and return a plot object
plotControls <- function(input, output, session,
  df_reactive, statistic_reactive, normalization_reactive,
  selected = list(
    logy = FALSE,
    markers = FALSE,
    legend = TRUE,
    plotAgainst = c("Date", "Days Since..."),
    forecast = FALSE
  )) {
  ns <- session$ns
  
  # render static UI elements...
  
  output$logyToggleUI <- renderUI(
    materialSwitch(ns("logyToggle"), status = "success", value = selected$logy,
      HTML("<span class='matswitchlabel'>Logarithmic</span>")))
  
  output$markersToggleUI <- renderUI(
    materialSwitch(ns("markersToggle"), status = "success", value = selected$markers,
      HTML("<span class='matswitchlabel'>Show Markers</span>")))
  
  output$legendToggleUI <- renderUI(
    materialSwitch(ns("legendToggle"), status = "success", value = selected$legend,
      HTML("<span class='matswitchlabel'>Show Legend</span>")))
  
  output$plotAgainstUI <- renderUI(
    selectInput(ns("plotAgainst"), "Plot Against",
      choices = c("Date", "Days Since..."), selected = selected$plotAgainst[1]))
  
  # create the "days since" selection UI
  observe({
    if (!is.null(input$plotAgainst) && input$plotAgainst == "Days Since...") {

      # set the number of ticks on the slider
      n_ticks <- 8

      # try to get equal numbers of points in each vertical slice
      unique_values <- uniqueValues(df_reactive())
      indices <- round(length(unique_values) / (n_ticks+1)) * 1:n_ticks
      choices <- formatC(unique_values[indices], format="g", digits=2)
      
      # dynamically create slider input label
      label <- "...cumulative"
      if (normalization_reactive() != "none") label <- paste(label, "normalized")
      label <- paste(label, statistic_reactive())
      
      output$daysSinceUI <- renderUI(
        sliderTextInput(ns("daysSince"), tolower(label), choices = choices, grid=T))
      
    } else output$daysSinceUI <- NULL
  })

  # save previously-selected plot line choice so it can be reselected below
  old_plotLine <- reactiveVal("Simple Linear Interpolation")
  
  # when the show markers toggle changes, we need to re-render the "plot line" UI
  observe({
    
    # FIX ME -- not sure if correct
    if (!is.null(input$plotLine)) old_plotLine(input$plotLine)
    
    choices <- c("Simple Linear Interpolation", "Best-Fit (Bi-)Logistic Curve")
    if (!is.null(input$markersToggle) && input$markersToggle) choices <- c(choices, "None")
    
    output$plotLineUI <- renderUI(
      selectInput(ns("plotLine"), "Plot Line",
        choices = choices,
        selected = old_plotLine()
      )
    )
  })
  
  # when the plot line selection changes, we need to re-render the "show forecast" UI
  observeEvent(input$plotLine, {
    output$forecastToggleUI <-
      if (input$plotLine == "Best-Fit (Bi-)Logistic Curve")
        renderUI(
          materialSwitch(ns("forecastToggle"), status="success", value = selected$forecast,
            HTML("<span class='matswitchlabel'>Show Forecast</span>")))
      else NULL
    
    # markers are only optional with a simple linear interpolation
    if (input$plotLine == "Simple Linear Interpolation") {
      shinyjs::show("markersToggle")
      
    } else {
      updateMaterialSwitch(session, "markersToggle", value = TRUE)
      shinyjs::hide("markersToggle")
    }
  })
  
  # this is the reactive plot object we'll return from this module
  plot_reactive <- reactiveVal(NULL)
  
  # when any inputs change, we need to re-render the plot
  observe({
    
    # get the data as a non-reactive data frame
    df <- df_reactive()
    countries <- rownames(df)
    
    # create plot only if user has selected at least one country
    if (length(countries) > 0) {
      
      # transpose data and add additional "Date" column
      if (input$plotAgainst == "Date") {
        
        # what to plot along the x-axis
        xval <- "Date"
        
        # transpose and add a Date column
        tf <- as.data.frame(t(df))
        tf$Date <- as.Date(rownames(tf))
        
        # complex row-wise shifting of data
      } else if (input$plotAgainst == "Days Since...") {
        
        # what to plot along the x-axis
        xval <- "Days Since..."
        minrate <- if (is.null(input$daysSince)) 0.1 else input$daysSince
        
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
        output$dataSelectionErrorUI <- NULL
        
        # show just lines? or markers and lines? or curve fit?
        mode <- if (input$plotLine == "Best-Fit (Bi-)Logistic Curve" || input$plotLine == "None") "markers"
                else if (input$markersToggle) "lines+markers"
                else "lines"
        
        # create plot with first country
        first_country <- countries[1]
        plot <- plot_ly(tf, x=tilde(xval), y=tilde(first_country), name=first_country,
          type="scatter", mode=mode)
        
        # add all additional countries in a loop
        if (len > 1) for (ii in 2:len) plot <- add_country(countries[ii], plot, mode)
        
        # create the dynamic plot title using the selected statistic and normalization
        statistic <- statistic_reactive()
        normalization <- normalization_reactive()
        
        # create the plot title / y-axis title
        plot_title <- HTML(paste0("Cumulative ",

          if (statistic == "confirmed") "Confirmed Cases"
          else if (statistic == "deaths") "Deaths"
          else if (statistic == "recovered") "Recovered",
          
          if (normalization == "none") ""
          else if (normalization == "population") " per capita"
          else if (normalization == "population-density") " per capita/km<sup>2</sup>"
          ))
        
        yaxis <- list(title=plot_title, tickprefix="   ")
        if (input$logyToggle) yaxis <- c(yaxis, type="log")
        
        plot <- plot %>% layout(
          title = plot_title,
          xaxis = list(title = xval),
          yaxis = yaxis,
          margin = list(l = 50, r = 50, b = 80, t = 80, pad = 20),
          showlegend = input$legendToggle
        )
        
        # if "Best-Fit Logistic Curve", add curve fits
        if (input$plotLine == "Best-Fit (Bi-)Logistic Curve")
          for (ii in 1:len) plot <- add_fit(tf, xval, countries[ii], plot, input$forecastToggle)
        
        plot_reactive(plot)
        
      } else {
        output$dataSelectionErrorUI <- renderUI(
          valueBox("Error", "No data to plot!", icon=icon("exclamation-circle"), color="red", width=12))
        
        plot_reactive(NULL)
      }
    }
  })

  # return the reactive plot object from this module whenever inputs change
  plot_reactive
}