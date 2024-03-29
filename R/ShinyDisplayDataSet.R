#' @import shiny
shinyDisplayDataSetUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator()

  verticalLayout(
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
        radioButtons(ns('dv'), tl('Dependent variable'), choices = c(''), inline = T),
        h4("dataset$initTable ..."), shiny2TableUI(ns("initTable")), br(),
        h4("dataset$dataTable ..."), shiny2TableUI(ns("dataTable"))
      ),
      type = "pills"
    )
  )
}

#' @import shiny
shinyDisplayDataSetMD <- function(id, dataset, exclude.from.others = c("fileTable","initTable","variables","symmetryTable","woutlierTable","dataTable")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tl <- getTranslator()

    observeEvent(dataset$isSetup, {
      req(dataset$isSetup)
      updateRadioButtons(session, 'dv', choices = names(dataset$initTable), selected = names(dataset$initTable)[1], inline = T)
    })

    observe({ if (req(dataset$isSetup)) {
      shiny2TableMD("initTable", dataset$initTable[[input$dv]], pageLength = 10, prefix = ns('initTable'))
      shiny2TableMD("dataTable", dataset$dataTable[[input$dv]], pageLength = 10, prefix = ns('dataTable'))
    } })

    output$fileTableTex <- renderPrint({ dataset$fileTable })
    output$variablesTex <- renderPrint({ dataset$variables })

    output$otherDataTex <- renderPrint({
      others <- list()
      for (var in names(dataset)[!names(dataset) %in% exclude.from.others]) {
        others[[var]] <- dataset[[var]]
      }
      others
    })
  })
}
