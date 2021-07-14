#' @import shiny
#' @export
compHypothesisUI <- function(id, test = id) {

  ns <- NS(id)
  tl <- getTranslator('compHypothesisTest')

  title <- 'Non-parametric Module'
  if ('kruskal' == test) {
    title <- paste0(tl("Kruskal-Wallis Module"),' (',tl("Alternative to One-Way Between-Subject ANOVA"),')')
  } else if ('srh' == test) {
    title <- paste0(tl("Scheirer-Ray-Hare Module"),' (',tl("Alternative to two and three between-subject ANOVA"),')')
  } else if ('wilcoxon' == test) {
    title <- paste0(tl("Wilcoxon Signed-Rank Module"),' (',tl("Alternative to T-test"),')')
  } else if ('ancova' == test) {
    title <- paste0(tl("ANCOVA Module"))
  }

  fluidPage(titlePanel(title), sidebarLayout(sidebarPanel(
    width = 3,
    loadDataSetUI(ns("loadData")),
    uiOutput(ns("settingSymmetryUI")),
    uiOutput(ns("settingOutliersUI")),
    uiOutput(ns("settingOthersUI"))
  ), mainPanel(width = 9, uiOutput(ns("mainPanelUI")))))

}

#' @import shiny
#' @export
compHypothesisMD <- function(id, test = id) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      tl <- getTranslator('compHypothesisTest')

      var.params <- list()
      if ('kruskal' == test) {
        var.params = list(
          between = list(type = "convert.non.numeric", max = 1, label = tl("columns for the between-subject factors (independent variables)"), removeFrom = c("wid")),
          dvs = list(type = "numeric", label = tl("columns for the dependent variables (outcomes)"), removeFrom = c("wid","between"))
        )
      } else if ('srh' == test) {
        var.params = list(
          between = list(type = "convert.non.numeric", min = 2, max = 3, label = tl("columns for the between-subject factors (independent variables)"), removeFrom = c("wid")),
          dvs = list(type = "numeric", label = tl("columns for the dependent variables (outcomes)"), removeFrom = c("wid","between"))
        )
      } else if ('wilcoxon' == test) {
        var.params = list(
          between = list(type = "non.numeric", max = 1, label = tl("column for the independent variable"), values.count = 2, removeFrom = c("wid")),
          dvs = list(type = "numeric", label = tl("columns for the dependent variables (outcomes)"), removeFrom = c("wid","between"))
        )
      } else if ('ancova' == test) {
        var.params = list(
          covar = list(type = "numeric", max = 1, label = tl("column for the covariate"), removeFrom = "wid"),
          between = list(type = "convert.non.numeric", max = 2, label = tl("columns for the between-subject factors (independent variables)"), removeFrom = c("wid","covar")),
          dvs = list(type = "numeric", label = tl("columns for the dependent variables (outcomes)"), removeFrom = c("wid","covar","between"))
        )
      }
      dataset <- loadDataSetMD('loadData', var.params = var.params, dv.vars = 'dvs'
                               , rds.signature = paste0(test,'-',as.character(packageVersion("rshinystatistics"))))

      # ... check conditions to update main panel

      check.redirect.call <- list(
        'none' = list(call.function = 'displayDataSetMD', call.params = list(id='dataSet', dataset=dataset)),
        'symmetry-outliers' = list(
          check = c('isSetup'), redirect = 'none',
          call.function = 'symmetryOutliersMD', call.params = list(id = 'symmetryOutliers', dataset = dataset, initTable = 'symmetryTable', dataTable='dataTable')
        ),
        'hypothesis' = list(
          check = c('isSetup', 'checkSymmetry', 'checkOutliers'), redirect = 'symmetry-outliers',
          call.function = 'doHypothesisMD', call.params = list(id = 'hypothesis', test = test, dataset = dataset)
        )
      )

      if ('ancova' == test) {
        check.redirect.call[['symmetry-outliers']] <- list(
          check = c('isSetup'), redirect = 'none',
          call.function = 'symmetryOutliersMD', call.params = list(id = 'symmetryOutliers', dataset = dataset, initTable = 'symmetryTable', dataTable='woutlierTable')
        )
        check.redirect.call[['normality']] <- list(
          check = c('isSetup','checkSymmetry','checkOutliers'), redirect = 'symmetry-outliers',
          call.function = 'normalityMD', call.params = list(id = 'normality', dataset = dataset, show.residual = T)
        )
        check.redirect.call[['linearity']] <- list(
          check = c('isSetup','checkSymmetry','checkOutliers','checkNormality'), redirect = 'normality',
          call.function = 'linearityMD', call.params = list(id = 'linearity', dataset = dataset)
        )
        check.redirect.call[['homogeneity']] <- list(
          check = c('isSetup','checkSymmetry','checkOutliers','checkNormality','checkLinearity'), redirect = 'linearity',
          call.function = 'homogeneityMD', call.params = list(id = 'homogeneity', dataset = dataset)
        )
        check.redirect.call[['hypothesis']] <- list(
          check = c('isSetup','checkSymmetry','checkOutliers','checkNormality','checkLinearity','checkHomogeneity'), redirect = 'homogeneity',
          call.function = 'doHypothesisMD', call.params = list(id = 'hypothesis', test = test, dataset = dataset)
        )
      }

      # ... setting interfaces

      output$settingOthersUI <- renderUI({
        if (!dataset$isSetup || 'ancova' != test) return(NULL)
        verticalLayout(
          uiOutput(ns('settingNormalityUI')),
          checkboxInput(ns('checkLinearity'), paste('(3)', tl('Linearity of data was checked'))),
          checkboxInput(ns('checkHomogeneity'), paste('(4)', tl('Homogeneity of data was checked')))
        )
      })

      output$mainPanelUI <- renderUI({
        if ('ancova' == test) {
          tabsetPanel(
            id = ns(paste0(test,"Panel")), type = "tabs", selected = "none"
            , tabPanel("DataSet", icon = icon("caret-right"), value = "none", displayDataSetUI(ns("dataSet")))
            , tabPanel(paste('(1)', tl("Assumption: Symmetry and Without Outliers")), value = "symmetry-outliers", symmetryOutliersUI(ns("symmetryOutliers")))
            , tabPanel(paste('(2)', tl("Assumption: Normality")), value = "normality", normalityUI(ns("normality")))
            , tabPanel(paste('(3)', tl("Assumption: Linearity")), value = "linearity", linearityUI(ns("linearity")))
            , tabPanel(paste('(4)', tl("Assumption: Homogeneity")), value = "homogeneity", homogeneityUI(ns("homogeneity")))
            , tabPanel(tl("Hypothesis Test"), value = "hypothesis", doHypothesisUI(ns("hypothesis"), test))
            , tabPanel(tl("Export"), value = "export-result", exportHypothesisUI(ns("export-result")))
          )
        } else {
          tabsetPanel(
            id = ns(paste0(test,"Panel")), type = "tabs", selected = "none"
            , tabPanel("DataSet", icon = icon("caret-right"), value = "none", displayDataSetUI(ns("dataSet")))
            , tabPanel(paste('(1)', tl("Assumption: Symmetry and Without Outliers")), value = "symmetry-outliers", symmetryOutliersUI(ns("symmetryOutliers")))
            , tabPanel(tl("Hypothesis Test"), value = "hypothesis", doHypothesisUI(ns("hypothesis"), test))
            , tabPanel(tl("Export"), value = "export-result", exportHypothesisUI(ns("export-result")))
          )
        }
      })

      # ... setting symmetry and dealing with outliers

      output$settingSymmetryUI <- renderUI({
        if (dataset$isSetup) settingSymmetryUI(ns("settingSymmetry"))
      })

      output$settingOutliersUI <- renderUI({
        if (dataset$isSetup) settingOutliersUI(ns("settingOutliers"))
      })

      output$settingNormalityUI <- renderUI({
        if ('ancova' == test)
          if (dataset$isSetup) settingNormalityUI(ns("settingNormality"))
      })

      settingSymmetry <- reactiveVal(NULL)
      settingOutliers <- reactiveVal(NULL)
      settingNormality <- reactiveVal(NULL)

      observeEvent(dataset$isSetup, {
        if (dataset$isSetup) {
          settingSymmetry(settingSymmetryMD("settingSymmetry", dataset, "dvs", initTable = 'initTable', dataTable = 'symmetryTable'))
          if ('ancova' == test) {
            settingOutliers(settingOutliersMD("settingOutliers", dataset, "dvs", "between", "covar", initTable = 'symmetryTable', dataTable = 'woutlierTable'))
            settingNormality(settingNormalityMD("settingNormality", dataset, "dvs", initTable = 'woutlierTable', dataTable = 'dataTable'))
          } else {
            settingOutliers(settingOutliersMD("settingOutliers", dataset, "dvs", "between", initTable = 'symmetryTable', dataTable = 'dataTable'))
          }
        } else {
          updateTabsetPanel(session, paste0(test,"Panel"), selected = "none")
          if (!is.null(settingNormality())) settingNormality()$normalityObserve$suspend()
          if (!is.null(settingOutliers())) settingOutliers()$outliersObserve$suspend()
          if (!is.null(settingSymmetry())) settingSymmetry()$skewnessObserve$suspend()
        }
      })

      observeEvent(input$checkLinearity, {
        if (dataset$isSetup) dataset$checkLinearity <- input$checkLinearity
      })

      observeEvent(input$checkHomogeneity, {
        if (dataset$isSetup) dataset$checkHomogeneity <- input$checkHomogeneity
      })

      # ... update dataTable

      observeEvent(input[[paste0(test,"Panel")]], {

        if (!dataset$isSetup) {
          updateTabsetPanel(session, paste0(test,"Panel"), selected = 'none')
          return(NULL)
        }
        tab <- isolate(input[[paste0(test,'Panel')]])

        margs <- check.redirect.call[[tab]]
        if (dataset$isSetup && 'export-result' == tab && !is.null(dataset[[paste0(test,'Params')]][["hypothesis"]])) {
          exportHypothesisMD("export-result", test, dataset)
        } else if (tab %in% names(check.redirect.call) && all(sapply(margs$check, FUN = function(check) (!is.null(dataset[[check]])  && dataset[[check]])))) {
          do.call(margs$call.function, margs$call.params)
        } else {
          showNotification(paste(tl("Before perform"), tab, ":", tl("you need to check all the previous assumptions")), type = "error")
          if (tab == 'export-result')
            updateTabsetPanel(session, paste0(test,"Panel"), selected = 'hypothesis')
          else
            updateTabsetPanel(session, paste0(test,"Panel"), selected = margs$redirect)
        }

      })

    }
  )
}
