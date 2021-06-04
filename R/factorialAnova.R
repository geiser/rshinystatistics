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
        , uiOutput(ns("settingSymmetryUI"))
        , uiOutput(ns("settingOutliersUI"))
        , uiOutput(ns("settingNormalityUI"))
        , uiOutput(ns("settingOthersUI"))
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("anovaPanel"), type = "tabs", selected = "none"
          , tabPanel("DataSet", icon = icon("caret-right"), value = "none", displayDataSetUI(ns("dataSet")))
          , tabPanel(paste('(1)', tl("Assumption: Symmetry and Without Outliers")), value = "symmetry-outliers", symmetryOutliersUI(ns("symmetryOutliers")))
          , tabPanel(paste('(2)', tl("Assumption: Normality")), value = "normality", normalityUI(ns("normality")))
          , tabPanel(paste('(3)', tl("Assumption: Homogeneity")), value = "homogeneity", homogeneityUI(ns("homogeneity")))
          , tabPanel(paste(tl("ANOVA Test")), value = "hypothesis", factorialAnovaHypothesisUI(ns("hypothesis")))
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

      output$settingOthersUI <- renderUI({
        if (!dataset$isSetup) return(NULL)
        verticalLayout(
          checkboxInput(ns('checkHomogeneity'), paste('(3)', tl('Homogeneity of data was checked')))
        )
      })

      # ... setting symmetry and dealing with outliers

      output$settingSymmetryUI <- renderUI({
        if (dataset$isSetup) settingSymmetryUI(ns("settingSymmetry"))
      })

      output$settingOutliersUI <- renderUI({
        if (dataset$isSetup) settingOutliersUI(ns("settingOutliers"))
      })

      output$settingNormalityUI <- renderUI({
        if (dataset$isSetup) settingNormalityUI(ns("settingNormality"))
      })

      settingSymmetry <- reactiveVal(NULL)
      settingOutliers <- reactiveVal(NULL)
      settingNormality <- reactiveVal(NULL)

      observeEvent(dataset$isSetup, {
        if (dataset$isSetup) {
          settingSymmetry(settingSymmetryMD("settingSymmetry", dataset, "dvs", initTable = 'initTable', dataTable = 'symmetryTable'))
          settingOutliers(settingOutliersMD("settingOutliers", dataset, "dvs", "between", initTable = 'symmetryTable', dataTable = 'woutlierTable'))
          settingNormality(settingNormalityMD("settingNormality", dataset, "dvs", initTable = 'woutlierTable', dataTable = 'dataTable'))
        } else {
          updateTabsetPanel(session, "anovaPanel", selected = "none")
          if (!is.null(settingNormality())) settingNormality()$normalityObserve$suspend()
          if (!is.null(settingOutliers())) settingOutliers()$outliersObserve$suspend()
          if (!is.null(settingSymmetry())) settingSymmetry()$skewnessObserve$suspend()
        }
      })

      observeEvent(input$checkHomogeneity, {
        if (dataset$isSetup) dataset$checkHomogeneity <- input$checkHomogeneity
      })

      # ... update dataTable

      observeEvent(input$anovaPanel, {
        if (!dataset$isSetup) {
          updateTabsetPanel(session, "anovaPanel", selected = "none")
          return(NULL)
        }
        tab <- isolate(input$anovaPanel)

        if (tab == 'none') {
          displayDataSetMD("dataSet", dataset, exclude.from.others = c("fileTable","initTable","variables","symmetryTable","woutlierTable"))
        } else if (tab == 'symmetry-outliers' && dataset$isSetup) {
          symmetryOutliersMD("symmetryOutliers", dataset, 'dvs', 'between', initTable = 'symmetryTable', dataTable = 'woutlierTable')
        } else if (tab == 'normality' && dataset$checkSymmetry && dataset$checkOutliers) {
          normalityMD("normality", dataset, 'dvs', 'between', show.residuals = T)
        } else if (tab == 'homogeneity' && dataset$checkSymmetry && dataset$checkOutliers && dataset$checkNormality) {
          homogeneityMD("homogeneity", dataset)
        } else if (tab == 'hypothesis' && dataset$checkSymmetry && dataset$checkOutliers && dataset$checkNormality && dataset$checkHomogeneity) {
          factorialAnovaHypothesisMD("hypothesis", dataset)
        } else if (tab == 'export-result' && !is.null(dataset$anovaParams[["hypothesis"]])) {
          factorialAnovaExportMD("export-result", dataset)
        } else {
          if (tab == 'normality') {
            showNotification(tl("Before checking the normality distribution, you need to assess the symmetry of data distribution and to perform a treatment of outliers"), type = "error")
            updateTabsetPanel(session, "anovaPanel", selected = "symmetry-outliers")
          }else if (tab == 'homogeneity') {
            showNotification(tl("Before checking the homogeneity, you need to check the normality distribution of data"), type = "error")
            updateTabsetPanel(session, "anovaPanel", selected = "normality")
          }else if (tab == 'hypothesis') {
            showNotification(tl("Before perform the hypothesis test, you need to check the homogeneity of data"), type = "error")
            updateTabsetPanel(session, "anovaPanel", selected = "homogeneity")
          } else if (tab == 'export-result') {
            showNotification(tl("Before export results, you need to perform ANOVA test"), type = "error")
            updateTabsetPanel(session, "anovaPanel", selected = "hypothesis")
          }
        }
      })
    }
  )
}

#' @import shiny
#' @export
factorialAnovaApp <- function() {
  shinyApp(ui = fluidPage(factorialAnovaUI("factorialAnovaApp")), server = function(input, output) {
    observe({ factorialAnovaMD("factorialAnovaApp") })
  })
}

