# get population density-normalized cumulative COVID deaths data
source("data.R")

library(DT)

tablePageUI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "tablePage", 
    fluidRow(
      column(width = 12,
        h2("Work in Progress :: Table Page")
      )
    ),
    fluidRow(
      column(width = 12,
        DT::dataTableOutput(ns("data_table"))
      )
    )
  )
}

tablePage <- function(input, output, session) {
  
  # get the data
  df_orig <- getData("deaths", "none")
  
  # transpose the data, add a Date column
  tf <- as.data.frame(t(df_orig))
  tf$Date <- as.Date(rownames(tf))
  
  # reverse order by Date, make Date first column
  tf <- tf[seq(dim(tf)[1],1), ]
  tf <- tf[ , c(ncol(tf), 1:(ncol(tf)-1))]
  
  output$data_table = DT::renderDataTable(
    DT::datatable(tf, class="nowrap hover display",
      rownames = FALSE,
      extensions = c("Buttons", "ColReorder"),
      options = list(
        scrollX = TRUE,
        dom = "Brtip",
        buttons = c("copy", "csv", "excel", "pdf", "print"),
        colReorder = list(fixedColumnsLeft = 1)
      )
    )
  )
  
}
