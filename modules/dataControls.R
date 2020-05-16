source("../functions/data.R", chdir=TRUE)

defaultDataControlsUI <- function(countriesUI, statisticUI, normalizationUI, rewindUI) {
  box(title = "Data Controls", status = "primary", width = NULL,
    solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
    
    fluidRow(
      column(width = 4, countriesUI),
      column(width = 4, statisticUI),
      column(width = 4, normalizationUI)
    ),
    fluidRow(
      column(width = 1),
      column(width = 10, rewindUI),
      column(width = 1)
    )
    
  )
}

dataControlsUI <- function(id, layout = defaultDataControlsUI) {
  ns <- NS(id)
  layout(
    uiOutput(ns("countriesUI")), 
    uiOutput(ns("statisticUI")), 
    uiOutput(ns("normalizationUI")), 
    uiOutput(ns("rewindUI"))
  )
}

# listen to the data-control inputs and return a reactive data.frame
dataControls <- function(input, output, session, selected = list(
    countries = c("Ireland", "US", "Italy", "United Kingdom", "Spain", "France", "Germany", "Japan"),
    statistic = "deaths",
    normalization = "population"
  )) {
  ns <- session$ns
  
  # time format used for dates in this data.frame
  timeFormat <- "%Y-%m-%d"

  # create the "statistic" selection UI
  output$statisticUI <- renderUI(
    selectInput(ns("statistic"), "Select Data",
      choices = statistics, selected = selected$statistic))

  # create the "normalization" selection UI
  output$normalizationUI <- renderUI(
    selectInput(ns("normalization"), "Normalize By",
      choices = normalizations, selected = selected$normalization))
  
  # ...other UIs must be rendered dynamically, according to
  # user selections for `statistic` and `normalization`, above

  # ...update when statistic or normalization selection changes
  df_reactive        <- reactiveVal(NULL) # all available data
  countries_reactive <- reactiveVal(NULL) # names of available countries
  dates_reactive     <- reactiveVal(NULL) # available dates
  retval_reactive    <- reactiveValues()  # a reactive subset of df_reactive, plus some meta info

  # when the statistic or normalization change, we need to query a new data frame
  observe({
    statistic     <- if (is.null(input$statistic))         statistics[1] else input$statistic
    normalization <- if (is.null(input$normalization)) normalizations[1] else input$normalization
    
    # set the return value
    retval_reactive$statistic = statistic
    retval_reactive$normalization = normalization

    # get the data frame and its row and column names
    df        <- getData(statistic, normalization)
    countries <- rownames(df)
    dates     <- colnames(df)
    
    # update available countries and dates
    countries_reactive(countries)
    dates_reactive(as.Date(dates, timeFormat))
    
    # update the reactive data frame
    df_reactive(df)
  })
  
  # if the user-selected countries or date range changes, we need to
  # subset the existing data frame, without necessarily querying for a new one
  observe({
    countries <- countries_reactive()
    dates     <- dates_reactive()
    
    # square the user-selected countries with the countries available in the df
    countries <- intersect(countries, input$countries)
    
    # get the first date of the data frame, and the user-selected last date
    firstDate <- which(dates==min(dates))
    lastDate  <- which(dates==input$rewind)
    
    if (length(lastDate) < 1) lastDate <- which(dates==max(dates))
    
    # subset the reactive data frame based on the above selections
    df <- df_reactive()
    retval_reactive$df = df[countries, firstDate:lastDate, drop=FALSE]
  })
  
  # save previously-selected countries so they can be reselected below
  old_countries <- reactiveVal(selected$countries)
  
  # ignoreInit so old_countries isn't set to NULL on initialization
  observeEvent(countries_reactive(), {
    old_countries(input$countries)
  }, ignoreInit = TRUE)
  
  # when the available countries change, we need to re-render the "country selection" UI
  observe({
    output$countriesUI <- renderUI(
      pickerInput(ns("countries"), "Select Countries",
        choices = sort(countries_reactive()),
        options = pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          liveSearchNormalize = TRUE,
          liveSearchPlaceholder = "Search",
          size = 10
        ),
        multiple = T,
        selected = intersect(countries_reactive(), old_countries()),
        width = "100%",
      )
    )
  })
  
  # save previously-selected end time so it can be reselected below
  old_maxDate <- reactiveVal(NULL)
  
  # when the available dates change, we need to re-render the "rewind" UI
  observe({
    
    # save the old selection before re-rendering the UI
    if (!is.null(input$rewind)) old_maxDate(input$rewind)
    
    dates <- dates_reactive()
    minDate <- min(dates)
    maxDate <- max(dates)
    
    newMaxDate <- old_maxDate()
    if (length(newMaxDate) < 1) newMaxDate <- maxDate
    if (newMaxDate < minDate)   newMaxDate <- minDate
    if (newMaxDate > maxDate)   newMaxDate <- maxDate
    
    output$rewindUI <- renderUI(
      sliderInput(ns("rewind"), "Rewind To",
        min = minDate, max = maxDate, value = newMaxDate,
        ticks = FALSE, timeFormat = timeFormat))
  })
  
  # return the subsetted reactive data frame from this module whenever inputs change
  retval_reactive
}