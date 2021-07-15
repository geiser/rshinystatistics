#' @import shiny
repMeasuresAnovaUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('repMeasuresAnova')

  fluidPage(
    titlePanel(tl("Repeated Measures Anova Module")),
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
          , tabPanel(tl("ANOVA Test"), value = "hypothesis", repAnovaHypothesisUI(ns("hypothesis")))
          #, tabPanel(tl("Export"), value = "export-result", repAnovaExportUI(ns("export-result")))
        )
      )
    )
  )
}

#' @import shiny
repMeasuresAnovaMD <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('repMeasuresAnova')

      dataset <- loadDataSetMD(
        "loadData",
        var.params = list(
          between = list(type = "convert.non.numeric", min = 0, max = 2,
                         label = tl("columns for the between-subject factors (independent variables)"),
                         removeFrom = c("wid")),
          within = list(type = "convert.non.numeric", min = 0, max = 2, max.depend.on = "between",
                        label = tl("columns for the within-subject factors (independent variables - same conditions in counterbalance design)"),
                        removeFrom = c("wid","between")),
          dvs = list(type = "repeated.measures", min = 1,
                     label = tl("Columns for the repeated-measures as dependent variable %i (outcomes)"),
                     removeFrom = c("wid","between","within"), default = "outcome")
        ),
        dv.vars = 'dvs',
        rds.signature = paste0('repMeasuresAnova-',as.character(packageVersion("rshinystatistics")))
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
            outliersMD("outliers", dataset, "dvs", c("between", "within", "dvs.within"))
          } else if (input$anovaPanel == 'normality') {
            normalityMD("normality", dataset, "dvs", "between", c("within","dvs.within"))
          } else if (input$anovaPanel == 'hypothesis') {
            repAnovaHypothesisMD("hypothesis", dataset, "dvs", "between", c("within","dvs.within"))
          } else if (input$anovaPanel == 'export-result' && !is.null(dataset$anovaParams[["hypothesis"]])) {
            #repAnovaExportMD("export-result", dataset, "dvs", "between", c("within","dvs.within"))
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

