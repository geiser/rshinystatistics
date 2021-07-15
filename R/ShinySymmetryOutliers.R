getOutliersBoxPlotly <- function(tbl, tbl2, dv, ivs, wid = 'row.pos', boxpoints = "none") {
  tl <- getTranslator()

  ivs <- intersect(ivs, colnames(tbl))
  if (length(ivs) == 0) {
    ivs <- c('iv')
    tbl[['iv']] <- rep('iv', nrow(tbl))
    tbl2[['iv']] <- tbl[['iv']]
  }
  livs <- as.list(ivs); names(livs) <- ivs

  lapply(livs, FUN = function(iv) {
    title <- paste0(tl('With outliers'),': ', dv, ' ~ ', iv)
    bxp <- boxPlotly(tbl, dv, iv, wid, boxpoints, title = title)

    title <- paste0(tl('Without outliers'),': ', dv, ' ~ ', iv)
    bxp_wo <- boxPlotly(tbl2, dv, iv, wid, boxpoints, title = title)
    return(list(plot = bxp, plot.wo = bxp_wo))
  })
}

#' @import shiny
shinySymmetryOutliersUI <- function(id) {

  ns <- NS(id)
  tl <- getTranslator()

  verticalLayout(
    br(), p(h4(tl("Descriptive Statistic"))), br()
    , shiny2TableUI(ns("symmetryAssessmentTbl")), br(), br()
    , uiOutput(ns("symmetryAnalysisUI")), br(), br()
    , uiOutput(ns("outliersAnalysisUI")), br(), br()
  )
}

#' @import shiny
shinySymmetryOutliersMD <- function(id, dataset, dvs = "dvs", ivs = "between", covar = NULL, initTable = 'initTable', dataTable="dataTable") {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      tl <- getTranslator()

      wid <- reactiveVal(dataset$variables$wid)
      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      rivs <- reactiveVal(unique(unlist(dataset$variables[c(ivs)], use.names = F)))

      rcovar <- reactiveVal(NULL)
      if (!is.null(covar) && length(covar) > 0)
        rcovar <- reactiveVal(unique(unlist(dataset$variables[c(covar)], use.names = F)))

      observeEvent(dataset$variables, {
        wid(dataset$variables$wid)
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
        rivs(unique(unlist(dataset$variables[c(ivs)], use.names = F)))
        if (!is.null(covar) && length(covar) > 0)
          rcovar(unique(unlist(dataset$variables[c(covar)], use.names = F)))
      })

      # ... update table of symmetry and outliers assessments

      updateSymmetryTables <- function() {
        if (!dataset$isSetup) return(NULL)

        idvs <- rdvs()
        data <- dataset[[dataTable]]
        if (!is.null(covar) && length(covar) > 0) {
          idvs <- c(rdvs(),rcovar())
          data[[rcovar()]] <- dataset[[dataTable]][[1]]
        }

        params <- list(data = data, dvs = idvs, ivs = rivs()
                       , type = 'mean_sd', include.global = T, symmetry.test = T, hide.details = T)
        df <- do.call(get.descriptives, params)
        shiny2TableMD("symmetryAssessmentTbl", df, prefix = ns('symmetry-assessment'))
      }

      observeEvent(dataset$isSetup, { if (dataset$isSetup) updateSymmetryTables() })
      observeEvent(dataset[[dataTable]], { if (dataset$isSetup) updateSymmetryTables() })

      # ... display information to help deal with non symmetry

      output$symmetryAnalysisUI <- renderUI({
        if (!dataset$isSetup) return(NULL)
        if (is.null(dataset$checkSymmetry) || dataset$checkSymmetry) return(NULL)

        verticalLayout(
          p(h4(tl("Suggestion to accomplish symmetry")), tl("Parameter test requires symmetry in the data distribution")),
          verbatimTextOutput(ns("symmetryAnalysisRes")),
          checkboxInput(ns("showSymmetryPlots"), tl("Show histograms with density plots to assess symmetry"), value = F, width = '100%'),
          conditionalPanel(
            condition = "input.showSymmetryPlots", ns=ns,
            fixedRow(
              column(width = 3, numericInput(ns("widthSymmetry"), "Width", value = 500, min=100, step = 50)),
              column(width = 3, numericInput(ns("heightSymmetry"), "Height", value = 400, min=100, step = 50)),
              column(width = 3, sliderInput(ns("binsSymmetry"), "Number of bins:", min = 5, max = 100, value = 35))
            ),
            uiOutput(ns("symmetryPlots"))
          )
        )
      })

      output$symmetryAnalysisRes <- renderText({
        if (!dataset$isSetup) return(NULL)

        idvs <- rdvs()
        data <- dataset[[dataTable]]
        if (!is.null(covar) && length(covar) > 0) {
          idvs <- c(rdvs(), rcovar())
          data[[rcovar()]] <- dataset[[dataTable]][[1]]
        }

        suggestions <- lapply(idvs, FUN = function(dv) {
          res <- symmetry.test(data[[dv]][[dv]])
          if (res$skewness.obs != "symmetrical (normal)") {
            paste0("As"," `", dv, "` ","is"," ", res$skewness.obs
                   , ", ","we recommend to apply "
                   , switch(res$skewness.obs
                            , 'positive moderate skew' = 'sqrt(x)'
                            , 'negative moderate skew' = 'sqrt(max(x+1)-x)'
                            , 'positive greater skew' = 'log10(x)'
                            , 'negative greater skew' = 'log10(max(x+1)-x)'
                            , 'positive severe skew' = '1/x'
                            , 'negative severe skew' = '1/(max(x+1)-x)'), " "
                   , "but you need to ensure this sugestion watching the histogram and density plots")
          }
        })
        suggestions[sapply(suggestions, is.null)] <- NULL

        do.call(paste, c(suggestions, sep='\n'))
      })

      output$symmetryPlots <- renderUI({
        if (!dataset$isSetup) return(NULL)
        if (is.null(input$showSymmetryPlots) || !input$showSymmetryPlots) return(NULL)

        idvs <- rdvs()
        data <- dataset[[dataTable]]
        if (!is.null(covar) && length(covar) > 0) {
          idvs <- c(rdvs(), rcovar())
          data[[rcovar()]] <- dataset[[dataTable]][[1]]
        }

        do.call(splitLayout, c(cellWidths = input$widthSymmetry, lapply(idvs, FUN = function(dv) {
          verticalLayout(
            strong(paste("Density plot of", dv)),
            renderPlot({
              gplot <- ggpubr::gghistogram(
                data[[dv]], x = dv, y = "..density..", add = "mean",
                bins = input$binsSymmetry, palette = "jco", rug = T, add_density = T)
              gplot <- gplot + ggpubr::stat_overlay_normal_density(color = "red", linetype = "dashed")
              gplot
            }, width = input$widthSymmetry, height = input$heightSymmetry)
          )
        })))
      })

      # ... display information to help deal with outliers

      output$outliersAnalysisUI <- renderUI({
        if (!dataset$isSetup) return(NULL)
        if (is.null(dataset$checkOutliers) || dataset$checkOutliers) return(NULL)

        boxchoices <- as.list(c("outliers", "suspectedoutliers", "all"))
        names(boxchoices) <- c(tl("Only outliers"), tl("Possible outliers"), tl("All points"))

        verticalLayout(
          p(h4(tl("Suggestion to deal with outliers")), tl("parameter tests require to avoid outliers to accomplish a normal distribution")),
          verbatimTextOutput(ns("outliersAnalysisRes")),
          checkboxInput(ns("showBoxPlots"), tl("Show box plots to identify outliers"), value = F, width = '100%'),
          conditionalPanel(
            condition = "input.showBoxPlots", ns=ns,
            fixedRow(
              column(width = 4, radioButtons(ns('boxdv'), tl('Dependent variable'), choices = rdvs(), selected = rdvs()[1], inline = T)),
              column(width = 5, radioButtons(ns("boxpoints"), tl("Point Display"), inline = T, choices = boxchoices, selected = boxchoices[1]))
            ),
            uiOutput(ns("boxPlots"))
          )
        )
      })

      output$outliersAnalysisRes <- renderText({
        if (!dataset$isSetup) return(NULL)

        data <- dataset[[dataTable]]
        suggestions <- lapply(rdvs(), FUN = function(dv) {

          outliers <- getOutliers(data[[dv]], dv, rivs())
          if (!is.null(covar) && length(covar) > 0) {
            outliers <- getOutliers(data[[dv]], dv, rivs(), rcovar())
          }

          if (!is.null(outliers) && nrow(outliers) > 0) {
            ids <- outliers[[wid()]][which(outliers$var == dv)]
            paste0("As"," `", dv,"` ","has outliers"," ", ", ","we recommend to deal with: ",paste0(ids, collapse = ","))
          }
        })
        suggestions[sapply(suggestions, is.null)] <- NULL
        do.call(paste,c(suggestions, sep='\n'))
      })

      output$boxPlots <- renderUI({
        if (!dataset$isSetup) return(NULL)
        if (is.null(input$showBoxPlots) || !input$showBoxPlots) return(NULL)

        dv <- input$boxdv
        dat <- dataset[[initTable]][[dv]]
        wo.dat <- dataset[[dataTable]][[dv]]
        plots <- getOutliersBoxPlotly(dat, wo.dat, dv, rivs(), wid(), input$boxpoints)
        verticalLayout(
          p(strong(paste(tl("Boxplot for variable"), dv))),
          do.call(verticalLayout, lapply(names(plots), FUN = function(iv) {
            splitLayout(plotly::renderPlotly({ plots[[iv]]$plot }),
                        plotly::renderPlotly({ plots[[iv]]$plot.wo }))
          }))
        )
      })

    }
  )
}


