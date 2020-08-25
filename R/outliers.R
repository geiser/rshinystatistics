
getOutliersBoxPlotly <- function(tbl, dvs, ivs, wid = 'row.pos', boxpoints = "none", outliers = list()) {

  tl <- getTranslator('outliers')
  ldvs <- as.list(dvs); names(ldvs) <- dvs
  livs <- as.list(ivs); names(livs) <- ivs

  lapply(ldvs, FUN = function(dv) {
    lapply(livs, FUN = function(iv) {
      dat <- tbl
      title <- paste0(tl('With outliers'),': ', dv, ' ~ ', iv)
      bxp <- boxPlotly(dat, dv, iv, wid, boxpoints, title = title)

      dat <- tbl[!tbl[[wid]] %in% c(outliers[[dv]]),]
      title <- paste0(tl('Without outliers'),': ', dv, ' ~ ', iv)
      bxp_wo <- boxPlotly(dat, dv, iv, wid, boxpoints, title = title)

      return(list(plot = bxp, plot.wo = bxp_wo))
    })
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
    df2TableUI(ns("outliersTable"))
  )
}

#' @import shiny
outliersMD <- function(id, dataset, dvs = 'dvs', ivs = 'ivs') {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('outliers')

      wid <- reactiveVal(dataset$variables$wid)
      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      rivs <- reactiveVal(unique(unlist(dataset$variables[c(ivs)], use.names = F)))

      observeEvent(dataset$variables, {
        wid(dataset$variables$wid)
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
        rivs(unique(unlist(dataset$variables[c(ivs)], use.names = F)))
      })

      # ... display box plots to identify outliers

      output$dataBoxPlotsUI <- renderUI({
        if (dataset$isSetup) {
          plots <- getOutliersBoxPlotly(dataset$initTable, rdvs(), rivs(),
                                        wid(), input$boxpoints, dataset$outliers)
          do.call(verticalLayout, lapply(names(plots), FUN = function(dv) {
            verticalLayout(
              br(), p(strong(paste(tl("Boxplot for variable"), dv))),
              do.call(verticalLayout, lapply(names(plots[[dv]]), FUN = function(iv) {
                splitLayout(plotly::renderPlotly({ plots[[dv]][[iv]]$plot }),
                            plotly::renderPlotly({ plots[[dv]][[iv]]$plot.wo }))
              }))
            )
          }))
        }
      })

      observeEvent(dataset$outliers, {
        if (!dataset$isSetup) return(NULL)
        df2TableMD("outliersTable", do.call(rbind, lapply(rdvs(), FUN = function(dv) {
          outliersIds <- c(dataset$outliers[[dv]])
          dat <- dataset$initTable[dataset$initTable[[wid()]] %in% outliersIds,]
          if (nrow(dat) > 0) return(cbind(var = dv, dat))
        })), c("var",wid(), rivs(), rdvs()), prefix = ns("outliers"))
      })

    }
  )
}
