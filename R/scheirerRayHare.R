#' @import shiny
#' @export
scheirerRayHareUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('scheirerRayHare')

  fluidPage(
    titlePanel(paste0(tl("Scheirer-Ray-Hare Module"),' (',tl("Alternative to two and three between-subject ANOVA"),')')),
    sidebarLayout(
      sidebarPanel(
        width = 3
        , loadDataSetUI(ns("loadData"))
        , uiOutput(ns("settingOutliersUI"))
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("srhPanel"), type = "tabs", selected = "none"
          , tabPanel("DataSet", icon = icon("caret-right"), value = "none", displayDataSetUI(ns("dataSet")))
          , tabPanel(tl("Assumption: Outliers (optional)"), value = "outliers", outliersUI(ns("outliers")))
          , tabPanel(tl("SRH Test"), value = "hypothesis", srhHypothesisUI(ns("hypothesis")))
          , tabPanel(tl("Export"), value = "export-result", srhExportUI(ns("export-result")))
        )
      )
    )
  )
}

#' @import shiny
#' @export
scheirerRayHareMD <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('scheirerRayHare')

      dataset <- loadDataSetMD(
        "loadData",
        var.params = list(
          between = list(type = "convert.non.numeric", min = 2, max = 3, label = tl("columns for the between-subject factors (independent variables)"), removeFrom = c("wid")),
          dvs = list(type = "numeric", label = tl("columns for the dependent variables (outcomes)"), removeFrom = c("wid","between"))
        ),
        dv.vars = 'dvs',
        rds.signature = paste0('srh-',as.character(packageVersion("rshinystatistics")))
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
          updateTabsetPanel(session, "srhPanel", selected = "none")
          if (!is.null(settingOutliers())) {
            settingOutliers()$outliersObserve$suspend()
          }
        }
      })

      # ... update dataTable

      observeEvent(input$srhPanel, {
        if (input$srhPanel == 'none') {
          displayDataSetMD("dataSet", dataset)
        } else if (dataset$isSetup) {
          if (input$srhPanel == 'outliers') {
            outliersMD("outliers", dataset, "dvs", "between")
          } else if (input$srhPanel == 'hypothesis') {
            srhHypothesisMD("hypothesis", dataset)
          } else if (input$srhPanel == 'export-result' && !is.null(dataset$srhParams[["hypothesis"]])) {
            srhExportMD("export-result", dataset)
          } else if (input$srhPanel == 'export-result') {
            showNotification(tl("Before export results, you need to perform Scheirer-Ray-Hare test"), type = "error")
            updateTabsetPanel(session, "srhPanel", selected = "hypothesis")
          }
        } else {
          updateTabsetPanel(session, "srhPanel", selected = "none")
        }
      })

    }
  )
}

#' @import shiny
#' @export
scheirerRayHareApp <- function() {
  shinyApp(ui = fluidPage(scheirerRayHareUI("ScheirerRayHare")), server = function(input, output) {
    scheirerRayHareMD("ScheirerRayHare")
  })
}

