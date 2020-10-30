library(DT)

tablePageUI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "tablePage", 
    fluidRow(
      column(width = 12,
        h3("Cumulative Deaths to Date")
      )
    ),
    fluidRow(
      column(width = 12,
        box(width = "100%",
          withSpinner(type = getOption("spinner.type", default = 7),
            DT::dataTableOutput(ns("data_table")))
        )
      )
    ),
    fluidRow(
      column(width = 12,
        dataControlsUI(ns("dataControls"))
      )
    )
  )
}

tablePage <- function(input, output, session) {

  # get the reactiveVal(data.frame) from the data controls module
  data_reactive <- callModule(dataControls, "dataControls")
  
  # extract the returned values from the dataControls module
  df_reactive <- reactive(data_reactive$df)
  
  # modify the reactive data frame so its easy to render
  dfr <- reactive({
    validate(need(nrow(df_reactive()) > 0, "No data available for selection"))
  
    # transpose the data, add a Date column
    tf <- as.data.frame(t(df_reactive()))
    tf$Date <- as.Date(rownames(tf))
  
    # reverse order by Date, make Date first column
    tf <- tf[seq(dim(tf)[1],1), ]
    tf <- tf[ , c(ncol(tf), 1:(ncol(tf)-1))]

    # round to at most 7 digits after the decimal point    
    tf[-1] <- lapply(tf[-1], round, 7)
    tf # (signif can be used to round to significant figures)
  })
  
  output$data_table = DT::renderDataTable({
    
    DT::datatable(dfr(), class="nowrap hover display",
      rownames = FALSE,
      extensions = c("Buttons", "ColReorder", "FixedColumns", "Scroller"),
      options = list(
        scrollX = TRUE,
        dom = "Brtip",
        colReorder = list(fixedColumnsLeft = 1),
        fixedColumns = TRUE,
        buttons = list(
          list(extend = "copy"),
          list(extend = "csv"),
          list(extend = "excel"),
          list(extend = "pdf"),
          list(extend = "print")
        ),
        deferRender = TRUE,
        scrollY = "55vh",
        scroller = TRUE
      )
    ) # %>% DT::formatSignif(colnames(dfr())[-1], 6)
  })
  
}
