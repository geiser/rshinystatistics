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
        , uiOutput(ns("settingSymmetryUI"))
        , uiOutput(ns("settingOutliersUI"))
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("wilcoxonPanel"), type = "tabs", selected = "none"
          , tabPanel("DataSet", icon = icon("caret-right"), value = "none", displayDataSetUI(ns("dataSet")))
          , tabPanel(paste('(1)', tl("Assumption: Symmetry and Without Outliers")), value = "symmetry-outliers", symmetryOutliersUI(ns("symmetryOutliers")))
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

      # ... setting symmetry and dealing with outliers

      output$settingSymmetryUI <- renderUI({
        if (dataset$isSetup) settingSymmetryUI(ns("settingSymmetry"))
      })

      output$settingOutliersUI <- renderUI({
        if (dataset$isSetup) settingOutliersUI(ns("settingOutliers"))
      })

      settingSymmetry <- reactiveVal(NULL)
      settingOutliers <- reactiveVal(NULL)

      observeEvent(dataset$isSetup, {
        if (dataset$isSetup) {
          settingSymmetry(settingSymmetryMD("settingSymmetry", dataset, "dvs", initTable = 'initTable', dataTable = 'symmetryTable'))
          settingOutliers(settingOutliersMD("settingOutliers", dataset, "dvs", "iv", initTable = 'symmetryTable', dataTable = 'dataTable'))
        } else {
          updateTabsetPanel(session, "wilcoxonPanel", selected = "none")
          if (!is.null(settingOutliers())) settingOutliers()$outliersObserve$suspend()
          if (!is.null(settingSymmetry())) settingSymmetry()$skewnessObserve$suspend()
        }
      })

      # ... update dataTable
      observeEvent(input$wilcoxonPanel, {
        if (!dataset$isSetup) {
          updateTabsetPanel(session, "wilcoxonPanel", selected = "none")
          return(NULL)
        }
        tab <- isolate(input$wilcoxonPanel)

        if (tab == 'none') {
          displayDataSetMD("dataSet", dataset, exclude.from.others = c("fileTable","initTable","variables","symmetryTable"))
        } else if (tab == 'symmetry-outliers' && dataset$isSetup) {
          symmetryOutliersMD("symmetryOutliers", dataset, 'dvs', 'iv', initTable = 'symmetryTable', dataTable = 'dataTable')
        } else if (tab == 'hypothesis' && dataset$checkSymmetry && dataset$checkOutliers && dataset$checkNormality && dataset$checkHomogeneity) {
          wilcoxonHypothesisMD("hypothesis", dataset)
        } else if (tab == 'export-result' && !is.null(dataset$wilcoxonParams[["hypothesis"]])) {
          wilcoxonExportMD("export-result", dataset)
        } else {
          if (tab == 'hypothesis') {
            showNotification(tl("Before performing the t-test, you need to perform the symmetry analysis and outliers treatment"), type = "error")
            updateTabsetPanel(session, "wilcoxonPanel", selected = "symmetry-outliers")
          } else if (tab == 'export-result') {
            showNotification(tl("Before export results, you need to perform T-test"), type = "error")
            updateTabsetPanel(session, "wilcoxonPanel", selected = "hypothesis")
          }
        }
      })
    }
  )
}

#' @import shiny
#' @export
wilcoxonTestApp <- function() {
  shinyApp(ui = fluidPage(wilcoxonTestUI("Wilcoxon")), server = function(input, output) {
    observe({ wilcoxonTestMD("Wilcoxon") })
  })
}

