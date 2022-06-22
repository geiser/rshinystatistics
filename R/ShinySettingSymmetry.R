set_symmetry <- function(dat, col, skew = NULL) {
  if (!is.null(skew) && (col %in% colnames(dat))) {
    if (skew == 'posSqrt') {
      dat[[paste0('std.',col)]] <- sqrt(dat[[col]])
    } else if (skew == 'negSqrt') {
      dat[[paste0('std.',col)]] <- -1*sqrt(max(dat[[col]]+1) - dat[[col]])
    } else if (skew == 'posLog') {
      dat[[paste0('std.',col)]] <- log10(dat[[col]]-(min(dat[[col]])-1))
    } else if (skew == 'negLog') {
      dat[[paste0('std.',col)]] <- -1*log10(max(dat[[col]]+1) - dat[[col]])
    } else if (skew == 'posInv') {
      dat[[paste0('std.',col)]] <- 1/(dat[[col]]-(min(dat[[col]])-1))
    } else  if (skew == 'negInv') {
      dat[[paste0('std.',col)]] <- -1/(max(dat[[col]]+1) - dat[[col]])
    }
  }
  return(dat)
}

#' @import shiny
shinySettingSymmetryUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator()
  verticalLayout(shinyjs::useShinyjs(), hr(), uiOutput(ns('skewnessInputUI')),
                 checkboxInput(ns('checkSymmetry'), paste('(1)', tl('Symmetry was checked'))))
}

#' @import shiny
shinySettingSymmetryMD <- function(id, dataset, dvs = "dvs", covar = 'covar', initTable = 'initTable', dataTable = 'dataTable') {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator()

      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      rcovar <- reactiveVal(unique(unlist(dataset$variables[c(covar)], use.names = F)))

      observeEvent(dataset$variables, {
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
        rcovar(unique(unlist(dataset$variables[c(covar)], use.names = F)))
      })

      # ... setting skewness inputs and its events

      output$skewnessInputUI <- renderUI({
        if (!dataset$isSetup) return(NULL)
        skewnessInputs <- lapply(c(rdvs(), rcovar()), FUN = function(dv) {
          choices <- as.list(c("none", "posSqrt", "negSqrt", "posLog", "negLog", "posInv", "negInv"))
          names(choices) <- c(tl("None"), "sqrt(x)", "sqrt(max(x+1)-x)","log10(x-(min(x)-1))","log10(max(x+1)-x)","1/(x-(min(x)-1))","1/(max(x+1)-x)")
          lbl <- paste0(tl('For symmetry in'),' "', dv, '", ',tl('apply'))
          selectInput(ns(paste0('skewness', dv, 'Input')), lbl, choices=choices, selected="none", multiple=F)
        })
        do.call(verticalLayout, skewnessInputs)
      })

      skewnessObserve <- observe({
        if (!dataset$isSetup) return(NULL)
        ldvs <- as.list(c(rdvs(), rcovar()))
        names(ldvs) <- c(rdvs(), rcovar())
        dataset$skewness <- lapply(ldvs, FUN = function(dv) {
          input[[paste0('skewness', dv, 'Input')]]
        })
      }, suspended = !dataset$isSetup)

      observeEvent(input$checkSymmetry, {
        dataset$checkSymmetry <- input$checkSymmetry
        for (dv in c(rdvs(), rcovar())) {
          shinyjs::enable(paste0('skewness', dv, 'Input'))
          if (dataset$checkSymmetry) shinyjs::disable(paste0('skewness',dv,'Input'))
        }
      })

      # ... setting events to update dataset

      updateData <- function() {
        if (!dataset$isSetup) return(NULL)
        skewnessObserve$suspend()

        ldvs <- as.list(rdvs())
        names(ldvs) <- rdvs()

        dataset[[dataTable]] <- lapply(ldvs, FUN = function(dv) {
          dat <- dataset[[initTable]][[dv]]
          if (length(rcovar()) > 0)
            dat <- set_symmetry(dat, rcovar(), skew = dataset$skewness[[rcovar()]])
          dat <- set_symmetry(dat, dv, skew = dataset$skewness[[dv]])
          if (!is.null(dat) && nrow(dat) > 0) return(dat)
        })

        skewnessObserve$resume()
      }

      observeEvent(dataset$skewness, { if (dataset$isSetup) updateData() })
      observeEvent(dataset[[initTable]], { if (dataset$isSetup) updateData() })
      observeEvent(dataset$isSetup, {
        skewnessObserve$suspend(); if (dataset$isSetup) skewnessObserve$resume()
      })

      return(list(session = session, skewnessObserve = skewnessObserve))
    }
  )
}


