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
        , uiOutput(ns("settingSymmetryUI"))
        , uiOutput(ns("settingOutliersUI"))
        , uiOutput(ns("settingNormalityUI"))
        , uiOutput(ns("settingOthersUI"))
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("indSampleTTestPanel"), type = "tabs", selected = "none"
          , tabPanel("DataSet", icon = icon("caret-right"), value = "none", displayDataSetUI(ns("dataSet")))
          , tabPanel(paste('(1)', tl("Assumption: Symmetry and Without Outliers")), value = "symmetry-outliers", symmetryOutliersUI(ns("symmetryOutliers")))
          , tabPanel(paste('(2)', tl("Assumption: Normality")), value = "normality", normalityUI(ns("normality")))
          , tabPanel(paste('(4)', tl("Assumption: Homogeneity")), value = "homogeneity", homogeneityUI(ns("homogeneity")))
          , tabPanel(paste(tl("T-Test")), value = "hypothesis", indSampleTTestHypothesisUI(ns("hypothesis")))
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
          settingOutliers(settingOutliersMD("settingOutliers", dataset, "dvs", "iv", initTable = 'symmetryTable', dataTable = 'woutlierTable'))
          settingNormality(settingNormalityMD("settingNormality", dataset, "dvs", initTable = 'woutlierTable', dataTable = 'dataTable'))
        } else {
          updateTabsetPanel(session, "indSampleTTestPanel", selected = "none")
          if (!is.null(settingNormality())) settingNormality()$normalityObserve$suspend()
          if (!is.null(settingOutliers())) settingOutliers()$outliersObserve$suspend()
          if (!is.null(settingSymmetry())) settingSymmetry()$skewnessObserve$suspend()
        }
      })

      observeEvent(input$checkHomogeneity, {
        if (dataset$isSetup) dataset$checkHomogeneity <- input$checkHomogeneity
      })

      # ... update dataTable
      observeEvent(input$indSampleTTestPanel, {
        if (!dataset$isSetup) {
          updateTabsetPanel(session, "indSampleTTestPanel", selected = "none")
          return(NULL)
        }
        tab <- isolate(input$indSampleTTestPanel)

        if (tab == 'none') {
          displayDataSetMD("dataSet", dataset, exclude.from.others = c("fileTable","initTable","variables","symmetryTable","woutlierTable"))
        } else if (tab == 'symmetry-outliers' && dataset$isSetup) {
          symmetryOutliersMD("symmetryOutliers", dataset, 'dvs', 'iv', initTable = 'symmetryTable', dataTable = 'woutlierTable')
        } else if (tab == 'normality' && dataset$checkSymmetry && dataset$checkOutliers) {
          normalityMD("normality", dataset, 'dvs', 'iv', show.residuals = T)
        } else if (tab == 'homogeneity' && dataset$checkSymmetry && dataset$checkOutliers && dataset$checkNormality) {
          homogeneityMD("homogeneity", dataset, 'dvs', 'iv')
        } else if (tab == 'hypothesis' && dataset$checkSymmetry && dataset$checkOutliers && dataset$checkNormality && dataset$checkHomogeneity) {
          indSampleTTestHypothesisMD("hypothesis", dataset)
        } else if (tab == 'export-result' && !is.null(dataset$indSampleTTestParams[["hypothesis"]])) {
          indSampleTTestExportMD("export-result", dataset)
        } else {
          if (tab == 'normality') {
            showNotification(tl("Before checking the normality distribution, you need to assess the symmetry of data distribution and to perform a treatment of outliers"), type = "error")
            updateTabsetPanel(session, "indSampleTTestPanel", selected = "symmetry-outliers")
          }else if (tab == 'homogeneity') {
            showNotification(tl("Before checking the homogeneity, you need to check the normality distribution of data"), type = "error")
            updateTabsetPanel(session, "indSampleTTestPanel", selected = "normality")
          }else if (tab == 'hypothesis') {
            showNotification(tl("Before perform the hypothesis test, you need to check the homogeneity of data"), type = "error")
            updateTabsetPanel(session, "indSampleTTestPanel", selected = "homogeneity")
          } else if (tab == 'export-result') {
            showNotification(tl("Before export results, you need to perform T-test"), type = "error")
            updateTabsetPanel(session, "indSampleTTestPanel", selected = "hypothesis")
          }
        }
      })
    }
  )
}

#' @import shiny
#' @export
indSampleTTestApp <- function() {
  shinyApp(ui = fluidPage(indSampleTTestUI("indSampleTTestApp")), server = function(input, output) {
    observe({ indSampleTTestMD("indSampleTTestApp") })
  })
}

