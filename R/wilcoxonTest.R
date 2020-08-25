#' @import shiny
#' @export
wilcoxonTestUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('wilcoxonTest')

  fluidPage(
    titlePanel(tl("Wilcoxon Signed-Rank Test Module")),
    sidebarLayout(
      sidebarPanel(
        width = 3
        , loadDataSetUI(ns("loadData"))
        , uiOutput(ns("settingOutliersUI"))
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("wilcoxonPanel"), type = "tabs", selected = "none"
          , tabPanel("DataSet", icon = icon("caret-right"), value = "none", displayDataSetUI(ns("dataSet")))
          , tabPanel(tl("Assumption: Outliers"), value = "outliers", outliersUI(ns("outliers")))
          , tabPanel(tl("Wilcoxon Test"), value = "hypothesis", wilcoxonHypothesisUI(ns("hypothesis")))
          , tabPanel(tl("Export"), value = "export-result", wilcoxonExportUI(ns("export-result")))
        )
      )
    )
  )
}

#' @import shiny
#' @export
wilcoxonTestMD <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('wilcoxonTest')

      dataset <- loadDataSetMD(
        "loadData",
        var.params = list(
          iv = list(type = "non.numeric", max = 1, label = tl("column for the independent variable"), values.count = 2, removeFrom = c("wid")),
          dvs = list(type = "numeric", label = tl("columns for the dependent variables (outcomes)"), removeFrom = c("wid","iv"))
        ),
        dv.vars = 'dvs',
        rds.signature = paste0('wilcoxon-',as.character(packageVersion("rshinystatistics")))
      )

      # ... setting outliers and setting normality panels

      output$settingOutliersUI <- renderUI({
        if (dataset$isSetup) settingOutliersUI(ns("settingOutliers"))
      })

      settingOutliers <- reactiveVal(NULL)

      observeEvent(dataset$isSetup, {
        if (dataset$isSetup) {
          settingOutliers(settingOutliersMD("settingOutliers", dataset, "dvs", "iv", updateDataTable = T))
        } else {
          updateTabsetPanel(session, "wilcoxonPanel", selected = "none")
          if (!is.null(settingOutliers())) settingOutliers()$outliersObserve$suspend()
        }
      })

      # ... update dataTable

      observeEvent(input$wilcoxonPanel, {
        if (input$wilcoxonPanel == 'none') {
          displayDataSetMD("dataSet", dataset)
        } else if (dataset$isSetup) {
          if (input$wilcoxonPanel == 'outliers') {
            outliersMD("outliers", dataset, "dvs", "iv")
          } else if (input$wilcoxonPanel == 'hypothesis') {
            wilcoxonHypothesisMD("hypothesis", dataset)
          } else if (input$wilcoxonPanel == 'export-result' && !is.null(dataset$wilcoxonParams[["hypothesis"]])) {
            wilcoxonExportMD("export-result", dataset)
          } else if (input$wilcoxonPanel == 'export-result') {
            showNotification(tl("Before export results, you need to perform Wilcoxon's Test"), type = "error")
            updateTabsetPanel(session, "wilcoxonPanel", selected = "hypothesis")
          }
        } else {
          updateTabsetPanel(session, "wilcoxonPanel", selected = "none")
        }
      })

    }
  )
}

#' @import shiny
#' @export
wilcoxonTestApp <- function() {
  shinyApp(ui = fluidPage(wilcoxonTestUI("Wilcoxon")), server = function(input, output) {
    wilcoxonTestMD("Wilcoxon")
  })
}

