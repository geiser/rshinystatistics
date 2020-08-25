#' @import shiny
displayDataSetUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('displayDataSet')

  verticalLayout(
    br(),
    tabsetPanel(
      tabPanel(
        tl("Setting Data"),
        h4("dataset$fileTable ..."),
        verbatimTextOutput(ns("fileTableTex")),

        h4("dataset$variables ..."),
        verbatimTextOutput(ns("variablesTex")),

        h4("Other Data inside dataset ..."),
        verbatimTextOutput(ns("otherDataTex"))
      ),
      tabPanel(
        tl("Generating Data"), icon = icon("table"),
        br(), h4("dataset$initTable ..."),df2TableUI(ns("initTable")),
        br(), h4("dataset$dataTable ..."), df2TableUI(ns("dataTable"))
      ),
      type = "pills"
    )
  )
}

#' @import shiny
displayDataSetMD <- function(id, dataset) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      tl <- getTranslator('displayDataSet')
      values <- reactiveValues()

      observeEvent(dataset$initTable, {
        cnames <- unlist(dataset$variables, use.names = F)
        df2TableMD("initTable", dataset$initTable, cnames, pageLength = 10, prefix = ns('initTable'))
      })

      observeEvent(dataset$dataTable, {
        cnames <- c('var', unlist(dataset$variables, use.names = F))
        df2TableMD("dataTable", dataset$dataTable, cnames, pageLength = 10, prefix = ns('dataTable'))
      })

      output$fileTableTex <- renderPrint({ dataset$fileTable })

      output$variablesTex <- renderPrint({ dataset$variables })

      output$otherDataTex <- renderPrint({
        others <- list()
        cnames <- c("fileTable", "initTable", "variables", "dataTable")
        for (var in names(dataset)[!names(dataset) %in% cnames]) {
          others[[var]] <- dataset[[var]]
        }
        others
      })

    }
  )
}
