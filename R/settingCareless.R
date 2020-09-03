#' @import shiny
#' @export
settingCarelessUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('settingOutliers')

  verticalLayout(
    hr(), h4(paste0(tl("Setting Careless"),':')),
    uiOutput(ns("carelessInputUI"))
  )
}

#' @import shiny
#' @export
settingCarelessMD <- function(id, dataset, items = "items", updateDataTable = T) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('settingOutliers')

      wid <- reactiveVal(dataset$variables$wid)
      ritems <- reactiveVal(unique(unlist(dataset$variables[c(items)], use.names = F)))

      observeEvent(dataset$variables, {
        wid(dataset$variables$wid)
        ritems(unique(unlist(dataset$variables[c(items)], use.names = F)))
      })

      # ... setting careless inputs

      output$carelessInputUI <- renderUI({
        if (!dataset$isSetup) return(NULL)
        choices <- dataset$initTable[[wid()]]
        selectInput(ns('carelessInput'), tl('Careless'), choices=choices, multiple=T)
      })

      carelessObserve <- observe({
        if (!dataset$isSetup) return(NULL)
        dataset$careless <- input[['carelessInput']]
      }, suspended = !dataset$isSetup)

      observeEvent(dataset$isSetup, {
        carelessObserve$suspend()
        if (dataset$isSetup) carelessObserve$resume()
      })

      # ... setting events to update dataTable

      observeEvent(dataset$careless, {
        if (!dataset$isSetup && updateDataTable) return(NULL)
        if (length(dataset$careless) > 0)
          ids <- dataset$careless
        dataset$dataTable <- dataset$initTable[!dataset$initTable[[wid()]] %in% ids,]
      })

      return(list(session = session, carelessObserve = carelessObserve))
    }
  )
}
