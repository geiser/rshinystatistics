#' @import shiny
#' @export
factorialAnovaUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('factorialAnova')

  fluidPage(
    titlePanel(tl("Factorial Anova Module")),
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
          id = ns("anovaPanel"), type = "tabs", selected = "none"
          , tabPanel("DataSet", icon = icon("caret-right"), value = "none", displayDataSetUI(ns("dataSet")))
          , tabPanel(tl("Assumption: Outliers"), value = "outliers", outliersUI(ns("outliers")))
          , tabPanel(tl("Assumption: Normality"), value = "normality", normalityUI(ns("normality")))
          , tabPanel(tl("Assumption: Homogeneity"), value = "homogeneity", homogeneityUI(ns("homogeneity")))
          , tabPanel(tl("ANOVA Test"), value = "hypothesis", factorialAnovaHypothesisUI(ns("hypothesis")))
          , tabPanel(tl("Export"), value = "export-result", factorialAnovaExportUI(ns("export-result")))
        )
      )
    )
  )
}

#' @import shiny
#' @export
factorialAnovaMD <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('factorialAnova')

      dataset <- loadDataSetMD(
        "loadData",
        var.params = list(
          between = list(type = "convert.non.numeric", max = 3, label = tl("columns for the between-subject factors (independent variables)"), removeFrom = c("wid")),
          dvs = list(type = "numeric", label = tl("columns for the dependent variables (outcomes)"), removeFrom = c("wid","between"))
        ),
        dv.vars = 'dvs',
        rds.signature = paste0('factorialAnova-',as.character(packageVersion("rshinystatistics")))
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
          settingOutliers(settingOutliersMD("settingOutliers", dataset, "dvs", "between", updateDataTable = F))
          settingNormality(settingNormalityMD("settingNormality", dataset, "dvs", updateDataTable = T))
        } else {
          updateTabsetPanel(session, "anovaPanel", selected = "none")
          if (!is.null(settingOutliers())) settingOutliers()$outliersObserve$suspend()
          if (!is.null(settingNormality())) {
            settingNormality()$skewnessObserve$suspend()
            settingNormality()$extremeObserve$suspend()
          }
        }
      })

      # ... update dataTable

      observeEvent(input$anovaPanel, {
        if (input$anovaPanel == 'none') {
          displayDataSetMD("dataSet", dataset)
        } else if (dataset$isSetup) {
          if (input$anovaPanel == 'outliers') {
            outliersMD("outliers", dataset, "dvs", "between")
          } else if (input$anovaPanel == 'normality') {
            normalityMD("normality", dataset)
          } else if (input$anovaPanel == 'homogeneity') {
            homogeneityMD("homogeneity", dataset)
          } else if (input$anovaPanel == 'hypothesis') {
            factorialAnovaHypothesisMD("hypothesis", dataset)
          } else if (input$anovaPanel == 'export-result' && !is.null(dataset$anovaParams[["hypothesis"]])) {
            factorialAnovaExportMD("export-result", dataset)
          } else if (input$anovaPanel == 'export-result') {
            showNotification(tl("Before export results, you need to perform ANOVA test"), type = "error")
            updateTabsetPanel(session, "anovaPanel", selected = "hypothesis")
          }
        } else {
          updateTabsetPanel(session, "anovaPanel", selected = "none")
        }
      })

    }
  )
}

#' @import shiny
#' @export
factorialAnovaApp <- function() {
  shinyApp(ui = fluidPage(factorialAnovaUI("factorialAnovaApp")), server = function(input, output) {
    factorialAnovaMD("factorialAnovaApp")
  })
}

