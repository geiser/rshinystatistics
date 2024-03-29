#' This function provides a wrapper for rstatic::identify_outliers for multiple dependent variables dvs
getOutliers <- function (data, dvs, ivs = c(), covar = c(), is.extreme = T, skewness = c()) {
  dat <- data

  if (length(skewness) > 0 && is.list(skewness))
    skewness = names(skewness)[sapply(skewness, FUN = function(i) !is.null(i) && i!='none')]

  if (length(ivs) > 0)
    dat <- dplyr::group_by_at(data, dplyr::vars(ivs))

  df.out <- do.call(rbind, lapply(dvs, FUN = function(dv) {
    if (dv %in% skewness) dv <- paste0(paste0('std.',dv))

    outliers <- rstatix::identify_outliers(dat, variable = dv, coef = 1.25)
    if (nrow(outliers) > 0 && length(covar) > 0) {
      if (covar %in% skewness) covar <- paste0(paste0('std.',covar))

      covarout <- rstatix::identify_outliers(dat, variable = covar, coef = 1.25)
      if (!is.null(covarout) && nrow(covarout) > 0) {
        cnames <- intersect(colnames(covarout), colnames(outliers))
        outliers <- plyr::rbind.fill(outliers[,cnames], covarout[,cnames])
      }
    }
    if (nrow(outliers) > 0) return(cbind(var = dv, outliers))
  }))

  if (is.extreme) return (df.out[df.out[["is.extreme"]] == is.extreme,])
  return(df.out)
}

#' @import shiny
shinySettingOutliersUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator()

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
    checkboxInput(ns('checkOutliers'), paste('(1)', tl('Outliers was treated')))
  )
}

#' @import shiny
shinySettingOutliersMD <- function(id, dataset, dvs = "dvs", ivs = "ivs", covar = "covar", initTable = 'initTable', dataTable = 'dataTable') {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator()

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
          outliers <- getOutliers(dataset[[initTable]][[dv]], dv, rivs(), rcovar(), skewness = dataset$skewness)
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
            dat <- winzorize(dat, dv, rivs(), rcovar(), skewness = dataset$skewness)
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
