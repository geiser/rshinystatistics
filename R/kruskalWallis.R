#' @import shiny
#' @export
kruskalWallisUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('kruskalWallis')

  fluidPage(
    titlePanel(paste0(tl("Kruskal-Wallis Module"),' (',tl("Alternative to One-Way Between-Subject ANOVA"),')')),
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
          id = ns("kruskalPanel"), type = "tabs", selected = "none"
          , tabPanel("DataSet", icon = icon("caret-right"), value = "none", displayDataSetUI(ns("dataSet")))
          , tabPanel(paste('(1)', tl("Assumption: Symmetry and Without Outliers")), value = "symmetry-outliers", symmetryOutliersUI(ns("symmetryOutliers")))
          , tabPanel(tl("Kruskal Test"), value = "hypothesis", kruskalHypothesisUI(ns("hypothesis")))
          , tabPanel(tl("Export"), value = "export-result", kruskalExportUI(ns("export-result")))
        )
      )
    )
  )
}

#' @import shiny
#' @export
kruskalWallisMD <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('kruskalWallis')

      dataset <- loadDataSetMD(
        "loadData",
        var.params = list(
          between = list(type = "convert.non.numeric", max = 1, label = tl("columns for the between-subject factors (independent variables)"), removeFrom = c("wid")),
          dvs = list(type = "numeric", label = tl("columns for the dependent variables (outcomes)"), removeFrom = c("wid","between"))
        ),
        dv.vars = 'dvs',
        rds.signature = paste0('kruskal-',as.character(packageVersion("rshinystatistics")))
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
          settingOutliers(settingOutliersMD("settingOutliers", dataset, "dvs", "between", initTable = 'symmetryTable', dataTable = 'dataTable'))
        } else {
          updateTabsetPanel(session, "kruskalPanel", selected = "none")
          if (!is.null(settingOutliers())) settingOutliers()$outliersObserve$suspend()
          if (!is.null(settingSymmetry())) settingSymmetry()$skewnessObserve$suspend()
        }
      })

      # ... update dataTable

      observeEvent(input$kruskalPanel, {
        if (!dataset$isSetup) {
          updateTabsetPanel(session, "kruskalPanel", selected = "none")
          return(NULL)
        }
        tab <- isolate(input$kruskalPanel)

        if (tab == 'none') {
          displayDataSetMD("dataSet", dataset, exclude.from.others = c("fileTable","initTable","variables","symmetryTable"))
        } else if (tab == 'symmetry-outliers' && dataset$isSetup) {
          symmetryOutliersMD("symmetryOutliers", dataset, 'dvs', 'between', initTable = 'symmetryTable', dataTable = 'dataTable')
        } else if (tab == 'hypothesis' && dataset$checkSymmetry && dataset$checkOutliers) {
          kruskalHypothesisMD("hypothesis", dataset)
        } else if (tab == 'export-result' && !is.null(dataset$srhParams[["hypothesis"]])) {
          kruskalExportMD("export-result", dataset)
        } else {
          if (tab == 'hypothesis') {
            showNotification(tl("Before perform the hypothesis test, you need to check the symmetry and outlieres of data"), type = "error")
            updateTabsetPanel(session, "kruskalPanel", selected = "symmetry-outliers")
          } else if (tab == 'export-result') {
            showNotification(tl("Before export results, you need to perform Scheirer-Ray-Hare test"), type = "error")
            updateTabsetPanel(session, "kruskalPanel", selected = "hypothesis")
          }
        }
      })

    }
  )
}

#' @import shiny
#' @export
kruskalWallisApp <- function() {
  shinyApp(ui = fluidPage(kruskalWallisUI("kruskal")), server = function(input, output, session) {
    observe({ kruskalWallisMD("kruskal") })
  })
}

