# The building blocks of Shiny apps:
#   `input` and `output`
#   observe() and observeEvent()
#   reactive() and reactiveEvent()
#   isolate()
#   reactiveVal() and reactiveValues()

# Shiny introduces an entirely new paradigm to R -- reactive programming.
# Reactive programs have reactive variables and reactive methods
# which can work alongside non-reactive variables and methods, and
# it can be confusing at the interface of these two paradigms. You'll
# often get errors which read "You tried to do something that can only
# be done from inside a reactive expression or observer." -- this means
# you've tried to improperly access a reactive element from a non-
# reactive context. We'll get to that.

# `input` values are sometimes called "reactive inputs" or "reactive
# sources". When an `input` value is included in the definition of a
# reactive conductor or a reactive endpoint, it is called a "reactive
# dependency" of that conductor or endpoint. `input` values are by far
# the most common kind of reactive variable, but they're not the only
# kind, see invalidateLater and reactiveTimer, for instance:
#   https://shiny.rstudio.com/reference/shiny/1.0.0/invalidateLater.html

# observe(), observeEvent(), reactive(), and reactiveEvent() can be
# thought of as functions that get their arguments solely through
# `input` values in their method bodies. They all "listen" for updates
# to their reactive dependencies and respond as soon as they are able
# by re-executing the entirety of their method body.

# reactive() and reactiveEvent() can be thought of as methods which
# return values, while observe() and observeEvent() do not. The former
# two are "reactive conductors" and the latter two are "reactive
# endpoints" or "reactive sinks".

# observe() and reactive() listen for updates to *any* of the reactive
# values contained in their method bodies, and will re-run whenever
# any of them update, unlike...

# observeEvent() and reactiveEvent(), which can use multiple reactive
# inputs and conductors in their bodies, but will only update when
# one or more specific ones -- which the user specifies -- update

# isolate() allows you to include a reactive variable in the body of
# an observe() or reactive() definition without making that variable
# a reactive dependency of that observe() or reactive(). So an
# isolate()d variable will not cause its enclosing observe() or
# reactive() block to re-execute. observeEvent() and reactiveEvent()
# blocks, then, can be thought of as observe() or reactive() blocks
# where nearly every varible is isolate()d.

library(shiny)
library(plotly)
library(ggplot2)

# check out different outputs and inputs on this Shiny cheatsheet
#   https://shiny.rstudio.com/images/shiny-cheatsheet.pdf

ui <- fluidPage(
  fluidRow(
    column(
      width = 6,
      fluidRow(column(width=12, checkboxInput("checkboxA", "A"))),
      fluidRow(column(width=12, checkboxInput("checkboxB", "B")))
    ),
    column(
      width = 6,
      fluidRow(column(width=12, p("reactive( f(A,B) )"),         verbatimTextOutput("reactiveAB"))),
      fluidRow(column(width=12, p("observe( f(A,B) )"),          verbatimTextOutput("observeAB"))),
      fluidRow(column(width=12, p("eventReactive( A, f(A,B) )"), verbatimTextOutput("reactiveEventAB"))),
      fluidRow(column(width=12, p("observeEvent( B, f(A,B) )"),  verbatimTextOutput("observeEventAB")))
    )
  )
)

server <- function(input, output) {
  
  myFunc <- function(a, b) paste("input$checkboxA + input$checkboxB =", a + b)
  
  output$reactiveAB <- reactive(myFunc(input$checkboxA, input$checkboxB))

  observe(output$observeAB <- renderText(myFunc(input$checkboxA, input$checkboxB)))
  
  output$reactiveEventAB <- eventReactive(input$checkboxA, { myFunc(input$checkboxA, input$checkboxB) })
  
  observeEvent(input$checkboxB, {
    output$observeEventAB <- renderText(myFunc(isolate(input$checkboxA), input$checkboxB))
    }, ignoreNULL = T, ignoreInit = F)

}

shinyApp(ui = ui, server = server)