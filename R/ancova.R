#' @import shiny
#' @export
ancovaUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('ancova')

  fluidPage(
    titlePanel(tl("Ancova Module")),
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
          id = ns("ancovaPanel"), type = "tabs", selected = "none"
          , tabPanel("DataSet", icon = icon("caret-right"), value = "none", displayDataSetUI(ns("dataSet")))
          , tabPanel(paste('(1)', tl("Assumption: Symmetry and Without Outliers")), value = "symmetry-outliers", symmetryOutliersUI(ns("symmetryOutliers")))
          , tabPanel(paste('(2)', tl("Assumption: Normality")), value = "normality", normalityUI(ns("normality")))
          , tabPanel(paste('(3)', tl("Assumption: Linearity")), value = "linearity", linearityUI(ns("linearity")))
          , tabPanel(paste('(4)', tl("Assumption: Homogeneity")), value = "homogeneity", homogeneityUI(ns("homogeneity")))
          , tabPanel(paste(tl("ANCOVA Test")), value = "hypothesis", ancovaHypothesisUI(ns("hypothesis")))
          , tabPanel(tl("Export"), value = "export-result", ancovaExportUI(ns("export-result")))
        )
      )
    )
  )
}

#' @import shiny
#' @export
ancovaMD <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('ancova')

      dataset <- loadDataSetMD(
        "loadData",
        var.params = list(
          covar = list(type = "numeric", max = 1, label = tl("column for the covariate"), removeFrom = "wid"),
          between = list(type = "convert.non.numeric", max = 2, label = tl("columns for the between-subject factors (independent variables)"), removeFrom = c("wid","covar")),
          dvs = list(type = "numeric", label = tl("columns for the dependent variables (outcomes)"), removeFrom = c("wid","covar","between"))
        ),
        dv.vars = 'dvs',
        rds.signature = paste0('ancova-',as.character(packageVersion("rshinystatistics")))
      )

      output$settingOthersUI <- renderUI({
        if (!dataset$isSetup) return(NULL)
        verticalLayout(
          checkboxInput(ns('checkLinearity'), paste('(3)', tl('Linearity of data was checked'))),
          checkboxInput(ns('checkHomogeneity'), paste('(4)', tl('Homogeneity of data was checked')))
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
          settingSymmetry(settingSymmetryMD("settingSymmetry", dataset, "dvs", "covar", initTable = 'initTable', dataTable = 'symmetryTable'))
          settingOutliers(settingOutliersMD("settingOutliers", dataset, "dvs", "between", "covar", initTable = 'symmetryTable', dataTable = 'woutlierTable'))
          settingNormality(settingNormalityMD("settingNormality", dataset, "dvs", initTable = 'woutlierTable', dataTable = 'dataTable'))
        } else {
          updateTabsetPanel(session, "ancovaPanel", selected = "none")
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

      observeEvent(input$ancovaPanel, {
        if (!dataset$isSetup) {
          updateTabsetPanel(session, "ancovaPanel", selected = "none")
          return(NULL)
        }
        tab <- isolate(input$ancovaPanel)

        if (tab == 'none') {
          displayDataSetMD("dataSet", dataset, exclude.from.others = c("fileTable","initTable","variables","symmetryTable","woutlierTable"))
        } else if (tab == 'symmetry-outliers' && dataset$isSetup) {
          symmetryOutliersMD("symmetryOutliers", dataset, 'dvs', 'between', initTable = 'symmetryTable', dataTable = 'woutlierTable')
        } else if (tab == 'normality' && dataset$checkSymmetry && dataset$checkOutliers) {
          normalityMD("normality", dataset, 'dvs', 'between', show.residuals = T)
        } else if (tab == 'linearity' && dataset$checkSymmetry && dataset$checkOutliers && dataset$checkNormality) {
          linearityMD("linearity", dataset)
        } else if (tab == 'homogeneity' && dataset$checkSymmetry && dataset$checkOutliers && dataset$checkNormality && dataset$checkLinearity) {
          homogeneityMD("homogeneity", dataset)
        } else if (tab == 'hypothesis' && dataset$checkSymmetry && dataset$checkOutliers && dataset$checkNormality && dataset$checkLinearity && dataset$checkHomogeneity) {
          ancovaHypothesisMD("hypothesis", dataset)
        } else if (tab == 'export-result' && !is.null(dataset$ancovaParams[["hypothesis"]])) {
          ancovaExportMD("export-result", dataset)
        } else {
          if (tab == 'normality') {
            showNotification(tl("Before checking the normality distribution, you need to assess the symmetry of data distribution and to perform a treatment of outliers"), type = "error")
            updateTabsetPanel(session, "ancovaPanel", selected = "symmetry-outliers")
          }else if (tab == 'linearity') {
            showNotification(tl("Before checking the linearity of covariance with dependent variable, you need to perform the normality distribution analysis"), type = "error")
            updateTabsetPanel(session, "ancovaPanel", selected = "normality")
          }else if (tab == 'homogeneity') {
            showNotification(tl("Before export results, you need to perform ANCOVA test"), type = "error")
            updateTabsetPanel(session, "ancovaPanel", selected = "linearity")
          }else if (tab == 'hypothesis') {
            showNotification(tl("Before export results, you need to perform ANCOVA test"), type = "error")
            updateTabsetPanel(session, "ancovaPanel", selected = "homogeneity")
          } else if (tab == 'export-result') {
            showNotification(tl("Before export results, you need to perform ANCOVA test"), type = "error")
            updateTabsetPanel(session, "ancovaPanel", selected = "hypothesis")
          }
        }
      })
    }
  )
}

#' @import shiny
#' @export
ancovaApp <- function() {
  shinyApp(ui = fluidPage(ancovaUI("ancovaApp")), server = function(input, output) {
    observer({ ancovaMD("ancovaApp") })
  })
}

