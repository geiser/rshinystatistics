
getOutliersBoxPlotly <- function(tbl, dv, ivs, wid = 'row.pos', boxpoints = "none", outliers = list()) {
  tl <- getTranslator('outliers')

  ivs <- intersect(ivs, colnames(tbl))
  if (length(ivs) == 0) {
    ivs <- c('iv')
    tbl[['iv']] <- rep('iv', nrow(tbl))
  }
  livs <- as.list(ivs); names(livs) <- ivs

  lapply(livs, FUN = function(iv) {
    dat <- tbl

    title <- paste0(tl('With outliers'),': ', dv, ' ~ ', iv)
    bxp <- boxPlotly(dat, dv, iv, wid, boxpoints, title = title)

    dat <- tbl[!tbl[[wid]] %in% c(outliers[[dv]]),]
    title <- paste0(tl('Without outliers'),': ', dv, ' ~ ', iv)
    bxp_wo <- boxPlotly(dat, dv, iv, wid, boxpoints, title = title)

    return(list(plot = bxp, plot.wo = bxp_wo))
  })
}

#' @import shiny
outliersUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('outliers')

  choices <- as.list(c("outliers", "suspectedoutliers", "all"))
  names(choices) <- c(tl("Only outliers"), tl("Possible outliers"), tl("All points"))

  verticalLayout(
    br(),
    radioButtons(ns("boxpoints"), tl("Point Display"), inline = T, choices = choices),
    uiOutput(ns("dataBoxPlotsUI")), br(), hr(),
    strong("Outliers"), p(tl("The following table lists all outliers")),
    radioButtons(ns('dv'), tl('Dependent variable'), choices = c(''), inline = T),
    df2TableUI(ns("outliersTable"))
  )
}

#' @import shiny
outliersMD <- function(id, dataset, dvs = 'dvs', ivs = 'ivs', table="initTable") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('outliers')

      wid <- reactiveVal(dataset$variables$wid)
      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      rivs <- reactiveVal(unique(unlist(dataset$variables[c(ivs)], use.names = F)))
      updateRadioButtons(session,'dv',choices = rdvs(), inline = T)

      observeEvent(dataset$variables, {
        wid(dataset$variables$wid)
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
        rivs(unique(unlist(dataset$variables[c(ivs)], use.names = F)))
        print(rdvs())
        updateRadioButtons(session,'dv',choices = rdvs(), inline = T)
      })

      # ... display box plots to identify outliers

      output$dataBoxPlotsUI <- renderUI({
        if (dataset$isSetup) {
          do.call(verticalLayout, lapply(rdvs(), FUN = function(dv) {
            plots <- getOutliersBoxPlotly(dataset[[table]][[dv]], dv, rivs(),
                                          wid(), input$boxpoints, dataset$outliers)
            verticalLayout(
              br(), p(strong(paste(tl("Boxplot for variable"), dv))),
              do.call(verticalLayout, lapply(names(plots), FUN = function(iv) {
                splitLayout(plotly::renderPlotly({ plots[[iv]]$plot }),
                            plotly::renderPlotly({ plots[[iv]]$plot.wo }))
              }))
            )
          }))
        }
      })

      observe({
        if (!is.null(dataset$isSetup) && dataset$isSetup) {
          outliersIds <- c(dataset$outliers[[input$dv]])
          dat <- dataset[[table]][[input$dv]]
          dat <- dat[dat[[wid()]] %in% outliersIds,]
          if (!is.null(dat))
            df2TableMD("outliersTable", dat, prefix = ns("outliers"))
        }
      })

    }
  )
}
