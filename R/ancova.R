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
        , uiOutput(ns("settingOutliersUI"))
        , uiOutput(ns("settingNormalityUI"))
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("ancovaPanel"), type = "tabs", selected = "none"
          , tabPanel("DataSet", icon = icon("caret-right"), value = "none", displayDataSetUI(ns("dataSet")))
          , tabPanel(tl("Assumption: Outliers"), value = "outliers", outliersUI(ns("outliers")))
          , tabPanel(tl("Assumption: Normality"), value = "normality", normalityUI(ns("normality")))
          , tabPanel(tl("Assumption: Linearity"), value = "linearity", linearityUI(ns("linearity")))
          , tabPanel(tl("Assumption: Homogeneity"), value = "homogeneity", homogeneityUI(ns("homogeneity")))
          , tabPanel(tl("ANCOVA Test"), value = "hypothesis", ancovaHypothesisUI(ns("hypothesis")))
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
          updateTabsetPanel(session, "ancovaPanel", selected = "none")
          if (!is.null(settingOutliers())) settingOutliers()$outliersObserve$suspend()
          if (!is.null(settingNormality())) {
            settingNormality()$skewnessObserve$suspend()
            settingNormality()$extremeObserve$suspend()
          }
        }
      })

      # ... update dataTabl

      observeEvent(input$ancovaPanel, {
        if (input$ancovaPanel == 'none') {
          displayDataSetMD("dataSet", dataset)
        } else if (dataset$isSetup) {
          if (input$ancovaPanel == 'outliers') {
            outliersMD("outliers", dataset, "dvs", "between")
          } else if (input$ancovaPanel == 'normality') {
            normalityMD("normality", dataset, only.residuals = T)
          } else if (input$ancovaPanel == 'linearity') {
            linearityMD("linearity", dataset)
          } else if (input$ancovaPanel == 'homogeneity') {
            homogeneityMD("homogeneity", dataset)
          } else if (input$ancovaPanel == 'hypothesis') {
            ancovaHypothesisMD("hypothesis", dataset)
          } else if (input$ancovaPanel == 'export-result' && !is.null(dataset$ancovaParams[["hypothesis"]])) {
            ancovaExportMD("export-result", dataset)
          } else if (input$ancovaPanel == 'export-result') {
            showNotification(tl("Before export results, you need to perform ANCOVA test"), type = "error")
            updateTabsetPanel(session, "ancovaPanel", selected = "hypothesis")
          }
        } else {
          updateTabsetPanel(session, "ancovaPanel", selected = "none")
        }
      })

    }
  )
}

#' @import shiny
#' @export
ancovaApp <- function() {
  shinyApp(ui = fluidPage(ancovaUI("ancovaApp")), server = function(input, output) {
    ancovaMD("ancovaApp")
  })
}

