#' @import shiny
#' @export
nonParamTestUI <- function(id, test = id) {

  ns <- NS(id)
  tl <- getTranslator('scheirerRayHare')

  title <- 'Non-parametric Module'
  if ('kruskal' == test) {
    title <- paste0(tl("Kruskal-Wallis Module"),' (',tl("Alternative to One-Way Between-Subject ANOVA"),')')
  } else if ('srh' == test) {
    title <- paste0(tl("Scheirer-Ray-Hare Module"),' (',tl("Alternative to two and three between-subject ANOVA"),')')
  } else if ('wilcoxon' == test) {
    title <- paste0(tl("Wilcoxon Signed-Rank Module"),' (',tl("Alternative to T-test"),')')
  }

  fluidPage(
    titlePanel(title),
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
          id = ns(paste0(test,"Panel")), type = "tabs", selected = "none"
          , tabPanel("DataSet", icon = icon("caret-right"), value = "none", displayDataSetUI(ns("dataSet")))
          , tabPanel(paste('(1)', tl("Assumption: Symmetry and Without Outliers")), value = "symmetry-outliers", symmetryOutliersUI(ns("symmetryOutliers")))
          , tabPanel(tl("Hypothesis Test"), value = "hypothesis", mdnHypothesisUI(ns("hypothesis"), test))
          , tabPanel(tl("Export"), value = "export-result", exportHypothesisTestUI(ns("export-result")))
        )
      )
    )
  )
}

#' @import shiny
#' @export
nonParamTestMD <- function(id, test = id) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      tl <- getTranslator('scheirerRayHare')

      dataset <- NULL
      if ('kruskal' == test) {
        dataset <- loadDataSetMD(
          "loadData",
          var.params = list(
            between = list(type = "convert.non.numeric", max = 1, label = tl("columns for the between-subject factors (independent variables)"), removeFrom = c("wid")),
            dvs = list(type = "numeric", label = tl("columns for the dependent variables (outcomes)"), removeFrom = c("wid","between"))
          ),
          dv.vars = 'dvs',
          rds.signature = paste0(test,'-',as.character(packageVersion("rshinystatistics")))
        )
      } else if ('srh' == test) {
        dataset <- loadDataSetMD(
          "loadData",
          var.params = list(
            between = list(type = "convert.non.numeric", min = 2, max = 3, label = tl("columns for the between-subject factors (independent variables)"), removeFrom = c("wid")),
            dvs = list(type = "numeric", label = tl("columns for the dependent variables (outcomes)"), removeFrom = c("wid","between"))
          ),
          dv.vars = 'dvs',
          rds.signature = paste0(test,'-',as.character(packageVersion("rshinystatistics")))
        )
      } else if ('wilcoxon' == test) {
        dataset <- loadDataSetMD(
          "loadData",
          var.params = list(
            between = list(type = "non.numeric", max = 1, label = tl("column for the independent variable"), values.count = 2, removeFrom = c("wid")),
            dvs = list(type = "numeric", label = tl("columns for the dependent variables (outcomes)"), removeFrom = c("wid","between"))
          ),
          dv.vars = 'dvs',
          rds.signature = paste0(test,'-',as.character(packageVersion("rshinystatistics")))
        )
      }

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
          updateTabsetPanel(session, paste0(test,"Panel"), selected = "none")
          if (!is.null(settingOutliers())) settingOutliers()$outliersObserve$suspend()
          if (!is.null(settingSymmetry())) settingSymmetry()$skewnessObserve$suspend()
        }
      })

      # ... update dataTable

      observeEvent(input[[paste0(test,"Panel")]], {
        if (!dataset$isSetup) {
          updateTabsetPanel(session, paste0(test,"Panel"))
          return(NULL)
        }
        tab <- isolate(input[[paste0(test,'Panel')]])

        if (tab == 'none') {
          displayDataSetMD("dataSet", dataset, exclude.from.others = c("fileTable","initTable","variables","symmetryTable"))
        } else if (tab == 'symmetry-outliers' && dataset$isSetup) {
          symmetryOutliersMD("symmetryOutliers", dataset, 'dvs', 'between', initTable = 'symmetryTable', dataTable = 'dataTable')
        } else if (tab == 'hypothesis' && dataset$checkSymmetry && dataset$checkOutliers) {
          mdnHypothesisMD("hypothesis", test, dataset)
        } else if (tab == 'export-result' && !is.null(dataset[[paste0(test,'Params')]][["hypothesis"]])) {
          exportHypothesisTestMD("export-result", test, dataset)
        } else {
          if (tab == 'hypothesis') {
            showNotification(tl("Before perform the hypothesis test, you need to check the symmetry and outlieres of data"), type = "error")
            updateTabsetPanel(session, paste0(test,"Panel"), selected = "symmetry-outliers")
          } else if (tab == 'export-result') {
            showNotification(tl(paste0("Before export results, you need to perform ",test," test")), type = "error")
            updateTabsetPanel(session, paste0(test,"Panel"), selected = "hypothesis")
          }
        }
      })

    }
  )
}

