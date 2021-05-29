#' @import shiny
settingOutliersUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('settingOutliers')

  verticalLayout(
    hr(), h4(paste0(tl("Setting Outliers"),':')),
    radioButtons(ns("method"), tl("How to deal with outliers?")
                 , choices = c("winsorize", "remove"), selected = "winsorize"),
    conditionalPanel(
      condition = "input.method == 'remove'", ns = ns,
      actionLink(ns("identifyingOutliers"), tl("Automatic identification of outliers")),
      uiOutput(ns("outliersInputUI"))
    )
  )
}

#' @import shiny
settingOutliersMD <- function(id, dataset, dvs = "dvs", ivs = "ivs", updateDataTable = T, identify.outliers = T) {
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

      observeEvent(input$method, {
        dataset$outlier.method <- input$method
      })

      observeEvent(input$identifyingOutliers, {
        if (!dataset$isSetup) return(NULL)
        for (dv in rdvs()) {
          outliers <- getOutliers(dataset$initTable[[dv]], dv, rivs())
          selected <- outliers[[wid()]][which(outliers$var == dv)]
          updateSelectInput(session, paste0('outliers', dv, 'Input'), selected=selected)
        }
      })

      # ... setting outliers inputs and its events

      output$outliersInputUI <- renderUI({
        if (!dataset$isSetup) return(NULL)
        outliersInputs <- lapply(rdvs(), FUN = function(dv) {
          choices <- dataset$initTable[[dv]][[wid()]]
          outliers <- getOutliers(dataset$initTable[[dv]], dv, rivs())
          selected <- outliers[[wid()]][which(outliers$var == dv)]
          if (!identify.outliers) selected <- c()
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
      updateDataTbl <- function() {
        if (!dataset$isSetup && updateDataTable) return(NULL)
        ldvs <- as.list(rdvs())
        names(ldvs) <- rdvs()
        dataset$dataTable <- lapply(ldvs, FUN = function(dv) {
          if (!is.null(dataset$outlier.method) && 'remove' == dataset$outlier.method) {
            ids <- c()
            if (length(dataset$outliers[[dv]]) > 0) ids <- dataset$outliers[[dv]]
            dat <- dataset$initTable[[dv]][!dataset$initTable[[dv]][[wid()]] %in% c(ids),]
          } else { # winsorize
            dat <- dataset$initTable[[dv]]
            dat[[dv]] <- DescTools::Winsorize(dat[[dv]], probs = c(0.05, 0.95))
          }
          if (!is.null(dat) && nrow(dat) > 0) return(dat)
        })
      }

      observeEvent(dataset$outlier.method, {
        updateDataTbl()
      })

      observeEvent(dataset$outliers, {
        updateDataTbl()
      })

      return(list(session = session, outliersObserve = outliersObserve))
    }
  )
}
