#' @import shiny
#' @export
pairedTTestUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('pairedTTest')

  fluidPage(
    titlePanel(tl("Paired Samples T-Test Module")),
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
          id = ns("pairedTTestPanel"), type = "tabs", selected = "none"
          , tabPanel("DataSet", icon = icon("caret-right"), value = "none", displayDataSetUI(ns("dataSet")))
          , tabPanel(tl("Assumption: Outliers"), value = "outliers", outliersUI(ns("outliers")))
          , tabPanel(tl("Assumption: Normality"), value = "normality", normalityUI(ns("normality")))
          , tabPanel(tl("T-Test"), value = "hypothesis", pairedTTestHypothesisUI(ns("hypothesis")))
          #, tabPanel(tl("Export"), value = "export-result", pairedTTestExportUI(ns("export-result")))
        )
      )
    )
  )
}

#' @import shiny
#' @export
pairedTTestMD <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('pairedTTest')

      dataset <- loadDataSetMD(
        "loadData",
        var.params = list(
          dvs = list(type = "repeated.measures", min = 1, max.measures = 2, min.measures = 2,
                     label = tl("Columns for the repeated-measures as dependent variable %i (outcomes)"),
                     removeFrom = c("wid"), default = "outcome")
        ),
        dv.vars = 'dvs', include.diffTable = T,
        rds.signature = paste0('pairedTTest-',as.character(packageVersion("rshinystatistics")))
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
          settingOutliers(settingOutliersMD("settingOutliers", dataset, "dvs", "iv", updateDataTable = F))
          settingNormality(settingNormalityMD("settingNormality", dataset, "dvs", updateDataTable = T))
        } else {
          updateTabsetPanel(session, "pairedTTestPanel", selected = "none")
          if (!is.null(settingOutliers())) settingOutliers()$outliersObserve$suspend()
          if (!is.null(settingNormality())) {
            settingNormality()$skewnessObserve$suspend()
            settingNormality()$extremeObserve$suspend()
          }
        }
      })

      observe({
        if (!is.null(dataset$diffTable) && dataset$isSetup) {
          ldvs <- as.list(names(dataset$diffTable))
          names(ldvs) <- names(dataset$diffTable)
          dataset$diffTable <- lapply(ldvs, FUN = function(dv) {
            wid <- dataset$variables$wid
            ids <- dataset$dataTable[[dv]][[wid]]

            dat <- dataset$diffTable[[dv]]
            skew <- isolate(dataset$skewness[[dv]])
            if (!is.null(skew)) {
              if (skew == 'posSqrt') {
                dat[[dv]] <- sqrt(dat[[dv]])
              } else if (skew == 'negSqrt') {
                dat[[dv]] <- sqrt(max(dat[[dv]]+1) - dat[[dv]])
              } else if (skew == 'posLog') {
                dat[[dv]] <- log10(dat[[dv]])
              } else if (skew == 'negLog') {
                dat[[dv]] <- log10(max(dat[[dv]]+1) - dat[[dv]])
              } else if (skew == 'posInv') {
                dat[[dv]] <- 1/(dat[[dv]])
              } else  if (skew == 'negInv') {
                dat[[dv]] <- 1/(max(dat[[dv]]+1) - dat[[dv]])
              }
            }

            return(dat[dat[[wid]] %in% ids,])
          })

        }
      })

      # ... update dataTable

      observeEvent(input$pairedTTestPanel, {
        if (input$pairedTTestPanel == 'none') {
          displayDataSetMD("dataSet", dataset)
        } else if (dataset$isSetup) {
          if (input$pairedTTestPanel == 'outliers') {
            outliersMD("outliers", dataset, "dvs.diff", table="diffTable")
          } else if (input$pairedTTestPanel == 'normality') {
            normalityMD("normality", dataset, "dvs.diff", only.residuals=T, table="diffTable")
          } else if (input$pairedTTestPanel == 'hypothesis') {
            pairedTTestHypothesisMD("hypothesis", dataset, "dvs", "dvs.within")
          #} else if (input$pairedTTestPanel == 'export-result' && !is.null(dataset$pairedTTestParams[["hypothesis"]])) {
          #  pairedTTestExportMD("export-result", dataset)
          } else if (input$pairedTTestPanel == 'export-result') {
            showNotification(tl("Before export results, you need to perform T-test"), type = "error")
            updateTabsetPanel(session, "pairedTTestPanel", selected = "hypothesis")
          }
        } else {
          updateTabsetPanel(session, "pairedTTestPanel", selected = "none")
        }
      })

    }
  )
}

#' @import shiny
#' @export
pairedTTestApp <- function() {
  shinyApp(ui = fluidPage(pairedTTestUI("pairedTTestApp")), server = function(input, output) {
    pairedTTestMD("pairedTTestApp")
  })
}

