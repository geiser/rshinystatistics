#' Winsorization
#'
#' This function performs the replacement of extreme values by less extreme ones
#' in a dataframe for performing parametric tests
#'
#' @param dat a data frame containing the values to be winzorized
#' @param dv column with the dependent variable
#' @param ivs columns with the independent variables
#' @param covar columns with the covariante
#' @param probs numeric vector of probabilities with values in [0,1] as used in quantile
#' @return A data frame containing the column with dv values replaced
#' @export
winzorize <- function(dat, dv, ivs = NULL, covar = NULL, probs = c(0.05, 0.95)) {
  if (dv %in% colnames(dat)) {
    dat[[dv]] <- DescTools::Winsorize(dat[[dv]], probs = probs)
    if (length(ivs) > 0) {
      pdat <- dplyr::group_by_at(dat, dplyr::vars(ivs))
      pdat <- dplyr::group_modify(pdat, function(.x,.y) {
        .x[[dv]] <- DescTools::Winsorize(.x[[dv]], probs = probs)
        if (length(covar) > 0) {
          .x[[covar]] <- DescTools::Winsorize(.x[[covar]], probs = probs)
        }
        return(.x)
      })
      dat <- as.data.frame(pdat)
    }
  }
  return(dat)
}


#' @import shiny
settingOutliersUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('settingOutliers')

  verticalLayout(
    shinyjs::useShinyjs(),
    hr(),
    radioButtons(ns("method"), tl("How to deal with outliers?")
                 , choices = c("none","winsorize","remove"), selected = "none", inline = T),
    conditionalPanel(
      condition = "input.method == 'remove'", ns = ns,
      actionLink(ns("identifyingOutliers"), tl("Automatic identification of outliers")),
      uiOutput(ns("outliersInputUI"))
    ),
    checkboxInput(ns('checkOutliers'), paste('(1)', tl('Outliers of data was treated')))
  )
}

#' @import shiny
settingOutliersMD <- function(id, dataset, dvs = "dvs", ivs = "ivs", covar = "covar", initTable = 'initTable', dataTable = 'dataTable') {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('settingOutliers')

      wid <- reactiveVal(dataset$variables$wid)
      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      rivs <- reactiveVal(unique(unlist(dataset$variables[c(ivs)], use.names = F)))
      rcovar <- reactiveVal(unique(unlist(dataset$variables[c(covar)], use.names = F)))

      observeEvent(dataset$variables, {
        wid(dataset$variables$wid)
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
        rivs(unique(unlist(dataset$variables[c(ivs)], use.names = F)))
        rcovar(unique(unlist(dataset$variables[c(covar)], use.names = F)))
      })

      # ... setting events for action link

      observeEvent(input$method, {
        dataset$outlier.method <- input$method
        if (input$method != 'remove') dataset$outliers <- NULL
      })

      observeEvent(input$identifyingOutliers, {
        if (!dataset$isSetup || input$method != 'remove') return(NULL)
        for (dv in rdvs()) {
          outliers <- getOutliers(dataset[[initTable]][[dv]], dv, rivs(), rcovar())
          selected <- outliers[[wid()]][which(outliers$var == dv)]
          updateSelectInput(session, paste0('outliers', dv, 'Input'), selected=selected)
        }
      })

      observeEvent(input$checkOutliers, {
        dataset$checkOutliers <- input$checkOutliers
        shinyjs::enable("method")
        if (input$checkOutliers) shinyjs::disable("method")
        if (input$method == 'remove') {
          for (dv in rdvs()) {
            shinyjs::enable(paste0('outliers', dv, 'Input'))
            if (input$checkOutliers) shinyjs::disable(paste0('outliers',dv,'Input'))
          }
        }
      })

      # ... setting outliers inputs and its events

      output$outliersInputUI <- renderUI({
        if ('remove' != input$method) return(NULL)
        outliersInputs <- lapply(rdvs(), FUN = function(dv) {
          selected <- c()
          choices <- dataset[[initTable]][[dv]][[wid()]]
          lbl <- paste0(tl('For normality in'),' "',dv,'", ', tl('remove'))
          selectInput(ns(paste0('outliers', dv, 'Input')), lbl, choices=choices, selected=selected, multiple=T)
        })
        do.call(verticalLayout, outliersInputs)
      })

      outliersObserve <- observe({
        if (!dataset$isSetup) return(NULL)
        if (is.null(input$method) || "remove" != input$method) return(NULL)
        ldvs <- as.list(rdvs()); names(ldvs) <- rdvs()
        dataset$outliers <- lapply(ldvs, FUN = function(dv) {
          input[[paste0('outliers', dv, 'Input')]]
        })
      }, suspended = !dataset$isSetup)

      # ... setting events to update dataTable

      updateDataTbl <- function() {
        if (!dataset$isSetup || is.null(input$method)) return(NULL)
        if ("remove" == input$method) outliersObserve$suspend()
        ldvs <- as.list(rdvs()); names(ldvs) <- rdvs()

        dataset[[dataTable]] <- lapply(ldvs, FUN = function(dv) {
          dat <- dataset[[initTable]][[dv]]
          if ('remove' == input$method) {
            ids <- c()
            if (length(dataset$outliers[[dv]]) > 0)
              ids <- dataset$outliers[[dv]]
            dat <- dat[!dat[[wid()]] %in% c(ids),]
          } else if ('winsorize' == input$method) {
            dat <- winzorize(dat, dv, rivs(), rcovar())
          }
          if (!is.null(dat) && nrow(dat) > 0) return(dat)
        })

        if ("remove" == input$method) outliersObserve$resume()
      }

      observeEvent(input$method, { if (dataset$isSetup) updateDataTbl() })
      observeEvent(dataset$outliers, { if (dataset$isSetup) updateDataTbl() })
      observeEvent(dataset[[initTable]], { if (dataset$isSetup) updateDataTbl() })
      observeEvent(dataset$isSetup, {
        outliersObserve$suspend(); if (dataset$isSetup) outliersObserve$resume()
      })

      return(list(session = session, outliersObserve = outliersObserve))
    }
  )
}
