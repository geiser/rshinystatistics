#' @import shiny
#' @export
shinyCarelessUI <- function(id) {

  ns <- NS(id)
  tl <- getTranslator()

  fluidPage(
    titlePanel(tl("Careless Response Module")),
    sidebarLayout(
      sidebarPanel(
        width = 3
        , shinyLoadDataSetUI(ns("loadData"))
        , uiOutput(ns("settingCarelessUI"))
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("carelessPanel"), type = "tabs", selected = "careless"
          , tabPanel("Careless", value = "careless", shinyDisplayCarelessUI(ns("careless")))
          #, tabPanel(tl("Export"), value = "export-result", span(ns("export-result")))
        )
      )
    )
  )
}

#' @import shiny
#' @export
shinyCarelessMD <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      tl <- getTranslator()

      dataset <- shinyLoadDataSetMD(
        "loadData",
        var.params = list(
          items = list(type = "any", min = 1,
                         label = tl("columns of items"),
                         removeFrom = c("wid"))
        ),
        rds.signature = paste0('careless-',as.character(packageVersion("rshinystatistics")))
      )

      # ... setting careless panels

      output$settingCarelessUI <- renderUI({
        if (dataset$isSetup) shinySettingCarelessUI(ns("settingCareless"))
      })

      settingCareless <- reactiveVal(NULL)

      observeEvent(dataset$isSetup, {
        if (dataset$isSetup) {
          settingCareless(shinySettingCarelessMD("settingCareless", dataset, "items", updateDataTable = T))
        } else {
          updateTabsetPanel(session, "carelessPanel", selected = "careless")
          if (!is.null(settingCareless()))
            settingCareless()$carelessObserve$suspend()
        }
      })

      # ... update careless Panel

      observeEvent(input$carelessPanel, {
        if (input$carelessPanel == 'careless') {
          shinyDisplayCarelessMD("careless", dataset)
        } else if (dataset$isSetup) {
          if (input$carelessPanel == 'export-result') {

          }
        } else {
          updateTabsetPanel(session, "anovaPanel", selected = "none")
        }
      })

    }
  )
}
