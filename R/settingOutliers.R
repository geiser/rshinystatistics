#' @import shiny
settingOutliersUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('settingOutliers')

  verticalLayout(
    hr(), h4(paste0(tl("Setting Outliers"),':')),
    actionLink(ns("identifyingOutliers"), tl("Automatic identification of outliers")),
    uiOutput(ns("outliersInputUI"))
  )
}

#' @import shiny
settingOutliersMD <- function(id, dataset, dvs = "dvs", ivs = "ivs", updateDataTable = T) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('settingOutliers')

      wid <- reactiveVal(dataset$variables$wid)
      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      rivs <- reactiveVal(unique(unlist(dataset$variables[c(ivs)], use.names = F)))

      observeEvent(dataset$variables, {
        wid(dataset$variables$wid)
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
        rivs(unique(unlist(dataset$variables[c(ivs)], use.names = F)))
      })

      # ... setting events for action link

      observeEvent(input$identifyingOutliers, {
        if (!dataset$isSetup) return(NULL)
        outliers <- getOutliers(dataset$initTable, rdvs(), rivs())
        for (dv in rdvs()) {
          selected <- outliers[[wid()]][which(outliers$var == dv)]
          updateSelectInput(session, paste0('outliers', dv, 'Input'), selected=selected)
        }
      })

      # ... setting outliers inputs and its events

      output$outliersInputUI <- renderUI({
        if (!dataset$isSetup) return(NULL)
        outliers <- getOutliers(dataset$initTable, rdvs(), rivs())
        outliersInputs <- lapply(rdvs(), FUN = function(dv) {
          choices <- dataset$initTable[[wid()]]
          selected <- outliers[[wid()]][which(outliers$var == dv)]
          lbl <- paste0(tl('Outliers for'),' "',dv,'"')
          selectInput(ns(paste0('outliers', dv, 'Input')), lbl, choices=choices, selected=selected, multiple=T)
        })
        do.call(verticalLayout, outliersInputs)
      })

      outliersObserve <- observe({
        if (!dataset$isSetup) return(NULL)
        ldvs <- as.list(rdvs()); names(ldvs) <- rdvs()
        dataset$outliers <- lapply(ldvs, FUN = function(dv) {
          input[[paste0('outliers', dv, 'Input')]]
        })
      }, suspended = !dataset$isSetup)

      observeEvent(dataset$isSetup, {
        outliersObserve$suspend()
        if (dataset$isSetup) outliersObserve$resume()
      })

      # ... setting events to update dataTable

      observeEvent(dataset$outliers, {
        if (!dataset$isSetup && updateDataTable) return(NULL)
        dataset$dataTable <- do.call(rbind, lapply(rdvs(), FUN = function(dv) {
          ids <- c()
          if (length(dataset$outliers[[dv]]) > 0) ids <- dataset$outliers[[dv]]
          dat <- dataset$initTable[!dataset$initTable[[wid()]] %in% c(ids),]
          if (!is.null(dat) && nrow(dat) > 0) return(cbind(var = dv, dat))
        }))
      })

      return(list(session = session, outliersObserve = outliersObserve))
    }
  )
}
