#' @import shiny
shinySettingNormalityUI <- function(id) {

  ns <- NS(id)
  tl <- getTranslator()

  verticalLayout(
    shinyjs::useShinyjs(),
    hr(),
    uiOutput(ns('extremeInputUI')),
    checkboxInput(ns('checkNormality'), paste('(2)', tl('Normality distribution was checked'))))
}

#' @import shiny
shinySettingNormalityMD <- function(id, dataset, dvs = "dvs", initTable = 'initTable', dataTable = 'dataTable') {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      tl <- getTranslator()

      wid <- reactiveVal(dataset$variables$wid)
      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))

      observeEvent(dataset$variables, {
        wid(dataset$variables$wid)
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      })

      # ... setting extreme inputs to be removed to achieve normality

      output$extremeInputUI <- renderUI({
        if (!dataset$isSetup) return(NULL)
        extremeInputs <- lapply(rdvs(), FUN = function(dv) {
          choices <- dataset[[initTable]][[dv]][[wid()]]
          selected <- isolate(dataset$toRemoveForNormality)[[dv]]
          lbl <- paste0(tl('Removing in'),' "',dv,'" ',tl('to achieve normality'))
          selectInput(ns(paste0('extreme', dv, 'Input')), lbl, choices=choices, selected=selected, multiple=T)
        })
        do.call(verticalLayout, extremeInputs)
      })

      extremeObserve <- observe({
        if (!dataset$isSetup) return(NULL)
        ldvs <- as.list(rdvs()); names(ldvs) <- rdvs()
        dataset$toRemoveForNormality <- lapply(ldvs, FUN = function(dv) {
          input[[paste0('extreme', dv, 'Input')]]
        })
      }, suspended = !dataset$isSetup)

      lapply(rdvs(), FUN = function(dv) {
        observeEvent(dataset$addToRemoveForNormality[[dv]], {
          if (!dataset$isSetup) return(NULL)
          if (length(dataset$addToRemoveForNormality[[dv]]) == 0) return(NULL)

          extremeObserve$suspend()
          ids <- c(isolate(dataset$addToRemoveForNormality)[[dv]],
                   isolate(input[[paste0('extreme', dv, 'Input')]]))
          updateSelectInput(session, paste0('extreme', dv, 'Input'), selected=ids)
          dataset$addToRemoveForNormality[[dv]] <- NULL
          extremeObserve$resume()
        })
      })

      observeEvent(input$checkNormality, {
        dataset$checkNormality <- input$checkNormality
        for (dv in rdvs()) {
          shinyjs::enable(paste0('extreme', dv, 'Input'))
          if (input$checkNormality) shinyjs::disable(paste0('extreme',dv,'Input'))
        }
      })

      # ... setting events to update dataset

      updateData <- function() {
        if (!dataset$isSetup) return(NULL)
        extremeObserve$suspend()
        ldvs <- as.list(rdvs()); names(ldvs) <- rdvs()

        dataset[[dataTable]] <- lapply(ldvs, FUN = function(dv) {

          ids <- c()
          toRemoveForNormality <- isolate(dataset$toRemoveForNormality)[[dv]]
          fromExtremeInputs <- isolate(input[[paste0('extreme', dv, 'Input')]])
          if (length(toRemoveForNormality) > 0) ids <- c(toRemoveForNormality, ids)
          if (length(fromExtremeInputs) > 0) ids <- c(fromExtremeInputs, ids)

          dat <- dataset[[initTable]][[dv]]
          if (length(unique(ids)) > 0 ) {
            dat <-  dat[!dat[[wid()]] %in% c(unique(ids)),]
          }
          if (!is.null(dat) && nrow(dat) > 0) return(dat)
        })

        extremeObserve$resume()
      }

      observeEvent(dataset$toRemoveForNormality, { if (dataset$isSetup) updateData() })
      observeEvent(dataset[[initTable]], { if (dataset$isSetup) updateData() })
      observeEvent(dataset$isSetup, {
        extremeObserve$suspend(); if (dataset$isSetup) extremeObserve$resume()
      })

      return(list(session = session, extremeObserve = extremeObserve))
    }
  )
}
