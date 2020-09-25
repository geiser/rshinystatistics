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
        , uiOutput(ns("settingOutliersUI"))
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("kruskalPanel"), type = "tabs", selected = "none"
          , tabPanel("DataSet", icon = icon("caret-right"), value = "none", displayDataSetUI(ns("dataSet")))
          , tabPanel(tl("Assumption: Outliers (optional)"), value = "outliers", outliersUI(ns("outliers")))
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

      # ... setting outliers and setting normality panels

      output$settingOutliersUI <- renderUI({
        if (dataset$isSetup) settingOutliersUI(ns("settingOutliers"))
      })

      settingOutliers <- reactiveVal(NULL)

      observeEvent(dataset$isSetup, {
        if (dataset$isSetup) {
          settingOutliers(settingOutliersMD("settingOutliers", dataset, "dvs", "between", updateDataTable = T, identify.outliers = F))
        } else {
          updateTabsetPanel(session, "kruskalPanel", selected = "none")
          if (!is.null(settingOutliers())) {
            settingOutliers()$outliersObserve$suspend()
          }
        }
      })

      # ... update dataTable

      observeEvent(input$kruskalPanel, {
        if (input$kruskalPanel == 'none') {
          displayDataSetMD("dataSet", dataset)
        } else if (dataset$isSetup) {
          if (input$kruskalPanel == 'outliers') {
            outliersMD("outliers", dataset, "dvs", "between")
          } else if (input$kruskalPanel == 'hypothesis') {
            kruskalHypothesisMD("hypothesis", dataset)
          } else if (input$kruskalPanel == 'export-result' && !is.null(dataset$kruskalParams[["hypothesis"]])) {
            kruskalExportMD("export-result", dataset)
          } else if (input$kruskalPanel == 'export-result') {
            showNotification(tl("Before export results, you need to perform Scheirer-Ray-Hare test"), type = "error")
            updateTabsetPanel(session, "kruskalPanel", selected = "hypothesis")
          }
        } else {
          updateTabsetPanel(session, "kruskalPanel", selected = "none")
        }
      })

    }
  )
}

#' @import shiny
#' @export
kruskalWallisApp <- function() {
  shinyApp(ui = fluidPage(kruskalWallisUI("KruskalWallis")), server = function(input, output) {
    kruskalWallisMD("KruskalWallis")
  })
}

