#' @import shiny
#' @export
indSampleTTestUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('indSampleTTest')

  fluidPage(
    titlePanel(tl("Independent Samples T-Test Module")),
    sidebarLayout(
      sidebarPanel(
        width = 3
        , loadDataSetUI(ns("loadData"))
        , uiOutput(ns("settingOutliersUI"))
        , uiOutput(ns("settingNormalityUI"))
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("indSampleTTestPanel"), type = "tabs", selected = "none"
          , tabPanel("DataSet", icon = icon("caret-right"), value = "none", displayDataSetUI(ns("dataSet")))
          , tabPanel(tl("Assumption: Outliers"), value = "outliers", outliersUI(ns("outliers")))
          , tabPanel(tl("Assumption: Normality"), value = "normality", normalityUI(ns("normality")))
          , tabPanel(tl("Assumption: Homogeneity"), value = "homogeneity", homogeneityUI(ns("homogeneity")))
          , tabPanel(tl("T-Test"), value = "hypothesis", indSampleTTestHypothesisUI(ns("hypothesis")))
          , tabPanel(tl("Export"), value = "export-result", indSampleTTestExportUI(ns("export-result")))
        )
      )
    )
  )
}

#' @import shiny
#' @export
indSampleTTestMD <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('indSampleTTest')

      dataset <- loadDataSetMD(
        "loadData",
        var.params = list(
          iv = list(type = "non.numeric", max = 1, label = tl("column for the independent variable"), values.count = 2, removeFrom = c("wid")),
          dvs = list(type = "numeric", label = tl("columns for the dependent variables (outcomes)"), removeFrom = c("wid","iv"))
        ),
        dv.vars = 'dvs',
        rds.signature = paste0('indSampleTTest-',as.character(packageVersion("rshinystatistics")))
      )

      # ... setting outliers and setting normality panels

      output$settingOutliersUI <- renderUI({
        if (dataset$isSetup) settingOutliersUI(ns("settingOutliers"))
      })

      output$settingNormalityUI <- renderUI({
        if (dataset$isSetup) settingNormalityUI(ns("settingNormality"))
      })

      settingOutliers <- reactiveVal(NULL)
      settingNormality <- reactiveVal(NULL)

      observeEvent(dataset$isSetup, {
        if (dataset$isSetup) {
          settingOutliers(settingOutliersMD("settingOutliers", dataset, "dvs", "iv", updateDataTable = F))
          settingNormality(settingNormalityMD("settingNormality", dataset, "dvs", updateDataTable = T))
        } else {
          updateTabsetPanel(session, "indSampleTTestPanel", selected = "none")
          if (!is.null(settingOutliers())) settingOutliers()$outliersObserve$suspend()
          if (!is.null(settingNormality())) {
            settingNormality()$skewnessObserve$suspend()
            settingNormality()$extremeObserve$suspend()
          }
        }
      })

      # ... update dataTable

      observeEvent(input$indSampleTTestPanel, {
        if (input$indSampleTTestPanel == 'none') {
          displayDataSetMD("dataSet", dataset)
        } else if (dataset$isSetup) {
          if (input$indSampleTTestPanel == 'outliers') {
            outliersMD("outliers", dataset, "dvs", "iv")
          } else if (input$indSampleTTestPanel == 'normality') {
            normalityMD("normality", dataset, "dvs", "iv")
          } else if (input$indSampleTTestPanel == 'homogeneity') {
            homogeneityMD("homogeneity", dataset, "dvs", "iv")
          } else if (input$indSampleTTestPanel == 'hypothesis') {
            indSampleTTestHypothesisMD("hypothesis", dataset)
          } else if (input$indSampleTTestPanel == 'export-result' && !is.null(dataset$indSampleTTestParams[["hypothesis"]])) {
            indSampleTTestExportMD("export-result", dataset)
          } else if (input$indSampleTTestPanel == 'export-result') {
            showNotification(tl("Before export results, you need to perform T-test"), type = "error")
            updateTabsetPanel(session, "indSampleTTestPanel", selected = "hypothesis")
          }
        } else {
          updateTabsetPanel(session, "indSampleTTestPanel", selected = "none")
        }
      })

    }
  )
}

#' @import shiny
#' @export
indSampleTTestApp <- function() {
  shinyApp(ui = fluidPage(indSampleTTestUI("indSampleTTestApp")), server = function(input, output) {
    indSampleTTestMD("indSampleTTestApp")
  })
}

