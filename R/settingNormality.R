#' @import shiny
settingNormalityUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('settingNormality')

  verticalLayout(
    hr(), h4(paste0(tl("Setting Normality"),':')),
    uiOutput(ns('skewnessInputUI')), br(),
    uiOutput(ns('extremeInputUI')), uiOutput(ns("updateDataTableUI"))
  )
}

#' @import shiny
settingNormalityMD <- function(id, dataset, dvs = "dvs", updateDataTable = T) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('settingNormality')

      wid <- reactiveVal(dataset$variables$wid)
      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))

      observeEvent(dataset$variables, {
        wid(dataset$variables$wid)
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      })

      # ... setting skewness inputs and its events

      output$skewnessInputUI <- renderUI({
        if (!dataset$isSetup) return(NULL)
        skewnessInputs <- lapply(rdvs(), FUN = function(dv) {
          choices <- as.list(c("none", "posSqrt", "negSqrt", "posLog", "negLog", "posInv", "negInv"))
          names(choices) <- c(tl("None"),
                              paste0("sqrt(x) ",tl("for positive moderate skew")),
                              paste0("sqrt(max(x+1)-x) ",tl("for negative moderate skew")),
                              paste0("log10(x) ",tl("for positive greater skew")),
                              paste0("log10(max(x+1)-x) ",tl("for negative greater skew")),
                              paste0("1/x ",tl("for positive severe skew")),
                              paste0("1/(max(x+1)-x) ",tl("for negative severe skew")))
          lbl <- paste0(tl('Transformation to avoid Skewness in'),' "',dv,'"')
          selectInput(ns(paste0('skewness', dv, 'Input')), lbl, choices=choices, selected="none", multiple=F)
        })
        do.call(verticalLayout, skewnessInputs)
      })

      skewnessObserve <- observe({
        if (!dataset$isSetup) return(NULL)
        ldvs <- as.list(rdvs()); names(ldvs) <- rdvs()
        dataset$skewness <- lapply(ldvs, FUN = function(dv) {
          input[[paste0('skewness', dv, 'Input')]]
        })
      }, suspended = !dataset$isSetup)

      # ... setting extreme inputs to be removed to achieve normality

      output$extremeInputUI <- renderUI({
        if (!dataset$isSetup) return(NULL)
        extremeInputs <- lapply(rdvs(), FUN = function(dv) {
          choices <- setdiff(dataset$initTable[[wid()]], c(dataset$outliers[[dv]]))
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

      # ... setting events to update dataset

      updateData <- function() {
        if (!dataset$isSetup || !updateDataTable) return(NULL)
        extremeObserve$suspend()
        skewnessObserve$suspend()

        dataset$dataTable <- do.call(rbind, lapply(rdvs(), FUN = function(dv) {
          ids <- c()
          outliers <- isolate(dataset$outliers)[[dv]]
          toRemoveForNormality <- isolate(dataset$toRemoveForNormality)[[dv]]
          fromExtremeInputs <- isolate(input[[paste0('extreme', dv, 'Input')]])
          if (length(outliers) > 0) ids <- c(outliers, ids)
          if (length(toRemoveForNormality) > 0) ids <- c(toRemoveForNormality, ids)
          if (length(fromExtremeInputs) > 0) ids <- c(fromExtremeInputs, ids)

          initTable <- isolate(dataset$initTable)
          dat <- initTable[!initTable[[wid()]] %in% c(unique(ids)),]

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
          if (!is.null(dat) && nrow(dat) > 0) return(cbind(var = dv, dat))
        }))

        skewnessObserve$resume()
        extremeObserve$resume()
      }

      observeEvent(dataset$outliers, { if (dataset$isSetup) updateData() })
      observeEvent(dataset$skewness, { if (dataset$isSetup) updateData() })
      observeEvent(dataset$toRemoveForNormality, { if (dataset$isSetup) updateData() })

      observeEvent(dataset$isSetup, {
        skewnessObserve$suspend(); extremeObserve$suspend()
        if (dataset$isSetup) { skewnessObserve$resume(); extremeObserve$resume() }
      })

      return(list(session = session, skewnessObserve = skewnessObserve, extremeObserve = extremeObserve))
    }
  )
}
