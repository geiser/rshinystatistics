#' @import shiny
repAnovaHypothesisUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('repAnovaHypothesis')

  mchoices <- list("Anova II" = 2, "Anova III" = 3, "Anova I (balanced)" = 1)
  echoices <- as.list(c("ges", "pes"))
  names(echoices) <- c("Generalized eta squared", "Partial eta squared")
  pchoices <- c("bonferroni", "hommel", "holm", "hochberg")
  addchoices <- list("todos" = "jitter", "média" = "mean", "não" = "none")

  verticalLayout(
    fixedRow(
      column(width = 6, verticalLayout(HTML(""))),
      column(width = 3, radioButtons(ns("type"), tl("Type AoV"), inline=F, choices=mchoices)),
      column(width = 3, radioButtons(ns("effect.size"), tl("Effect size"), inline=F, choices=echoices))
    ),
    fixedRow(
      column(width = 2, actionButton(ns("performTest"), tl("Perform/Update Test"), icon = icon("running"))),
      column(width = 6, h4(tl("Results from ANOVA test"))),
    ),
    h4(tl("Result from ANOVA")), shiny2TableUI(ns("result")), br(),
    h4(tl("Pairwise Comparisons")), br(),
    radioButtons(ns("p.adjust.method"), tl("P-value ajust method"), choices = pchoices, selected = pchoices[1], inline = T, width = "100%"),
    shiny2TableUI(ns("pairwise")), br(),
    h4(tl("Descriptive Statistics")), shiny2TableUI(ns("descr.statistics")), br(), hr(),
    radioButtons(ns("dv"), tl("Y-axis variable"), choices = c("dv"), inline = T, width = "100%"),
    fixedRow(
      column(width = 3, radioButtons(ns("addParam"),  "point style", inline = T, choices = addchoices)),
      column(width = 2, numericInput(ns("width"), "width", value = 700, min = 100, step = 50)),
      column(width = 2, numericInput(ns("height"), "height", value = 700, min = 100, step = 50)),
      column(width = 2, numericInput(ns("font.label.size"), tl("Font text size"), value = 14, min = 4, step = 2)),
      column(width = 2, numericInput(ns("step.increase"), tl("Step of signif."), value = 0.25, min = 0.05, max = 0.95, step = 0.05)),
      column(width = 1, actionButton(ns("updatePlot"), tl("Update plot")))
    ),
    uiOutput(ns("pairwisePlotsUI"))
  )
}


#' @import shiny
repAnovaHypothesisMD <- function(id, dataset, dvs = "dvs", between = "between", within = "within") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('repAnovaHypothesis')

      wid <- reactiveVal(dataset$variables$wid)
      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      rbetween <- reactiveVal(unique(unlist(dataset$variables[c(between)], use.names = F)))
      rwithin <- reactiveVal(unique(unlist(dataset$variables[c(within)], use.names = F)))
      updateRadioButtons(session, "dv", choices = rdvs(), selected = rdvs()[1], inline = T)

      observeEvent(dataset$variables, {
        wid(dataset$variables$wid)
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
        rbetween(unique(unlist(dataset$variables[c(between)], use.names = F)))
        rwithin(unique(unlist(dataset$variables[c(within)], use.names = F)))
        updateRadioButtons(session, "dv", choices = rdvs(), selected = rdvs()[1], inline = T)
      })

      values <- reactiveValues()

      # .. update ANOVA results

      updateResult <- function() {
        if (dataset$isSetup) {
          values$aov <- get.anova.test(dataset$dataTable, rdvs(), rbetween(), rwithin(), wid = wid(), type = input$type, effect.size = input$effect.size, dv.var = 'var')
          values$anova.test <- get.anova.table(values$aov)
          values$pwc <- get.anova.pwc(dataset$dataTable, rdvs(), rbetween(), rwithin(), p.adjust.method = input$p.adjust.method, dv.var = 'var')
          values$pair.wise <- get.anova.pwc.table(values$pwc)
        }
      }

      observeEvent(input$performTest, {
        if (!dataset$isSetup) return(NULL)
        if (dataset$isSetup) {
          updateResult()

          cname1 <- c("var", "Effect", "DFn", "DFd", "SSn", "SSd", "F", "p", input$effect.size, "p.signif")
          shiny2TableMD("result", values$anova.test, cname1, prefix = ns('result'))

          cname2 <- c("var", rbetween(), rwithin(),"group1","group2","df","statistic","p", "p.adj","p.adj.signif")
          shiny2TableMD("pairwise", values$pair.wise, cname2, pageLength = 50, prefix=ns('pairwise'))


          ds.df <- descriptives(dataset$dataTable, rdvs(), c(rbetween(),rwithin()), "common", dv.var = "var")
          cname3 <- c("var",rbetween(),rwithin(),"n","emmean","se.emms","conf.low","conf.high","mean","median","sd","ci")
          shiny2TableMD("descr.statistics", ds.df, cname3, prefix=ns("descr.statistics"))

          # ... update dataset anova parameters
          if (!'anovaParams' %in% names(dataset)) dataset$anovaParams <- list()
          dataset$anovaParams[["hypothesis"]] <- list(
            type = input$type,
            effect.size = input$effect.size,
            p.adjust.method = input$p.adjust.method
          )
          dataset$aov <- values$aov
          dataset$pwc <- values$pwc
        }
      })

      # ... displays plots

      observeEvent(input$updatePlot, {
        if (!dataset$isSetup) return(NULL)
        output$pairwisePlotsUI <- renderUI({
          if (!dataset$isSetup) return(NULL)

          dv <- isolate(input$dv)
          dat <- as.data.frame(dataset$dataTable[[dv]])
          ivs <- intersect(c(isolate(rbetween()), isolate(rwithin())), colnames(dat))

          width <- isolate(input$width)
          height <- isolate(input$height)
          addParam <- isolate(input$addParam)
          font.label.size <- isolate(input$font.label.size)
          step.increase <- isolate(input$step.increase)


          # ... update dataset anova parameters
          if (!'anovaParams' %in% names(dataset)) dataset$anovaParams <- list()
          if (!'plot' %in% names(dataset$anovaParams)) dataset$anovaParams[["plot"]] <- list()
          dataset$anovaParams[["plot"]][[dv]] <- list(
            width = width, height = height, font.label.size = font.label.size,
            addParam = addParam, step.increase = step.increase
          )

          # ... plots anova results from pairwise
          plots <- list()
          if (length(ivs) == 1)
            plots <- oneWayAnovaPlots(dat, dv, ivs, values$aov[[dv]], values$pwc[[dv]], addParam=addParam,
                                      font.label.size = font.label.size, step.increase = step.increase)
          else if (length(ivs) == 2)
            plots <- twoWayAnovaPlots(dat, dv, ivs, values$aov[[dv]], values$pwc[[dv]], addParam=addParam,
                                       font.label.size = font.label.size, step.increase = step.increase)
          else if (length(ivs) == 3)
            plots <- threeWayAnovaPlots(dat, dv, ivs, values$aov[[dv]], values$pwc[[dv]], addParam=addParam,
                                        font.label.size = font.label.size, step.increase = step.increase)

          if (length(ivs) == 3) {
            do.call(verticalLayout, lapply(names(plots), FUN = function(iv) {
              do.call(verticalLayout, lapply(names(plots[[iv]]), FUN = function(grpby) {
                if (!is.null(plots[[iv]][[grpby]])) {
                  verticalLayout(
                    h4(paste0('Plot of "',dv,'" based on "',iv,'" and grouped by "',grpby,'"',
                              paste0(' (color: ',setdiff(ivs,c(grpby,iv)),')'))),
                    renderPlot({ plots[[iv]][[grpby]] }, width = width, height = height))
                }
              }))
            }))
          } else if (length(ivs) == 2) {
            do.call(verticalLayout, lapply(names(plots), FUN = function(iv) {
              if (!is.null(plots[[iv]])) {
                verticalLayout(
                  h4(paste0('Plot of "',dv,'" based on "',iv,'"', paste0(' (color: ',setdiff(ivs,iv),')'))),
                  renderPlot({ plots[[iv]] }, width = width, height = height))
              }
            }))
          } else {
            do.call(verticalLayout, lapply(names(plots), FUN = function(iv) {
              if (!is.null(plots[[iv]])) {
                verticalLayout(
                  h4(paste0('Plot of "',dv,'" based on "',iv,'"')),
                  renderPlot({ plots[[iv]] }, width = width, height = height))
              }
            }))
          }

        })
      })

    }
  )
}
