#' @import shiny
shinyDoHypothesisUI <- function(id, test) {

  ns <- NS(id)
  tl <- getTranslator()

  opt.name <- "pwc.method"
  opt.label <- tl("Pairwise comparison method")
  opt.choices <- list("Wilcoxon's test"="wilcoxon")

  pairwiseCompLayout <- verticalLayout()
  if (test %in% c('wilcoxon', 'ttest')) {
    opt.name <- "alternative"
    opt.label <- tl("Alternative hypothesis")
    opt.choices <- as.list(c('two.sided', 'greater', 'less'))
    names(opt.choices) <- c(tl('Two tailed'), tl('Greater than'), tl('Less than'))
  } else if (test %in% c('ancova','anova')) {
    opt.name <- "type"
    opt.label <- tl("Type AoV")
    opt.choices <- as.list(c(2, 3, 1))
    names(opt.choices) <- c(tl('Anova II'), tl('Anova III'), tl('Anova I (balanced)'))
    pchoices <- c("bonferroni", "hommel", "holm", "hochberg")
    pairwiseCompLayout <- verticalLayout(
      h4(tl("Pairwise Comparisons")), br(),
      radioButtons(ns("p.adjust.method"), tl("P-value ajust method"), choices = pchoices, selected = pchoices[1], inline = T, width = "100%"),
      shiny2TableUI(ns("pairwise")), br(), hr()
    )
  } else {
    pchoices <- c("bonferroni", "hommel", "holm", "hochberg")
    pairwiseCompLayout <- verticalLayout(
      h4(tl("Pairwise Comparisons")), br(),
      radioButtons(ns("p.adjust.method"), tl("P-value ajust method"), choices = pchoices, selected = pchoices[1], inline = T, width = "100%"),
      shiny2TableUI(ns("pairwise")), br(), hr()
    )
  }

  addchoices <- list("todos" = "jitter", "média" = "mean", "não" = "none")

  title.emms.ds <- tl("Descriptive Statistics")
  if (test %in% c('anova','ancova'))
    title.emms.ds <- tl("Descriptive Statistics of Estimated Marginal Means")

  verticalLayout(
    fixedRow(
      column(width = 6, verticalLayout(HTML(""))),
      column(width = 3, radioButtons(ns(opt.name), opt.label, choices=opt.choices, inline=F)),
      column(width = 3, uiOutput(ns('secondParamUI')))
    ),
    fixedRow(
      column(width = 2, actionButton(ns("performTest"), tl("Perform/Update Test"), icon = icon("running"))),
      column(width = 6, h4(tl("Results from the hypothesis test"))),
    ),
    shiny2TableUI(ns("result")), br(), hr(),
    pairwiseCompLayout,
    h4(title.emms.ds), shiny2TableUI(ns("emms.ds.tbl")), br(), hr(),
    radioButtons(ns("dv"), tl("Y-axis variable"), choices = c("dv"), inline = T, width = "100%"),
    fixedRow(
      column(width = 3, radioButtons(ns("addParam"),  "point style", inline = T, choices = addchoices)),
      column(width = 2, numericInput(ns("width"), "width", value = 700, min = 100, step = 50)),
      column(width = 2, numericInput(ns("height"), "height", value = 700, min = 100, step = 50)),
      column(width = 2, numericInput(ns("font.label.size"), tl("Font text size"), value = 14, min = 4, step = 2)),
      column(width = 2, numericInput(ns("step.increase"), tl("Step of signif."), value = 0.25, min = 0.05, max = 0.95, step = 0.05)),
      column(width = 1, actionButton(ns("updatePlot"), tl("Update Plot")))
    ),
    uiOutput(ns("pairwisePlotsUI"))
  )
}

#' @import shiny
shinyDoHypothesisMD <- function(id, test, dataset, dvs = "dvs", between = "between", covar = "covar", dataTable = "dataTable") {

  opt.name <- "pwc.method"
  if (test %in% c('ancova','anova')) opt.name <- "type"
  if (test %in% c('wilcoxon','ttest')) opt.name <- "alternative"

  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator()

      wid <- reactiveVal(dataset$variables$wid)
      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      rbetween <- reactiveVal(unique(unlist(dataset$variables[c(between)], use.names = F)))
      rcovar <- reactiveVal(unique(unlist(dataset$variables[c(covar)], use.names = F)))

      observeEvent(dataset$variables, {
        if (!dataset$isSetup) return(NULL)
        wid(dataset$variables$wid)
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
        rbetween(unique(unlist(dataset$variables[c(between)], use.names = F)))
        rcovar(unique(unlist(dataset$variables[c(covar)], use.names = F)))
        updateRadioButtons(session, "dv", choices = rdvs(), selected = rdvs()[1], inline = T)
      })

      values <- reactiveValues()

      # ... user interfaces

      output$secondParamUI <- renderUI({
        if (!dataset$isSetup) return(NULL)
        if (test %in% c('ancova', 'anova')) {
          radioButtons(ns("effect.size"), tl("Effect size"), inline=F, choices=c("ges", "pes"), selected = 'ges')
        } else if (test == 'ttest') {
          verticalLayout(
            radioButtons(ns("vareq"), tl("Method"), inline = T, choices = list("Welch" = FALSE, "Student" = TRUE), selected = F),
            radioButtons(ns("hedge"), "Hedges' effsize", inline = T, choices = list("Yes" = TRUE, "Not" = FALSE), selected = T)
          )
        }
      })

      # ... update the hypothesis results

      updateResult <- function() {
        if (!dataset$isSetup) return(NULL)
        if ('wilcoxon' == test) {
          list.wtest <- wilcoxon.test(dataset$dataTable, rdvs(), rbetween(), input[[opt.name]], as.list = T)
          values$wt <- list.wtest$wt
          values$ez <- list.wtest$ez
          values$wilcoxon <- list.wtest$wilcoxon.test
          values$wilcoxon.test <- list.wtest$wilcoxon.test
        } else if ('ttest' == test) {
          list.ttest <- ind.ttest(dataset$dataTable, rdvs(), rbetween(), input[[opt.name]], as.logical(input$vareq), as.logical(input$hedge), as.list = T)
          values$tt <- list.ttest$tt
          values$ez <- list.ttest$ez
          values$ttest <- list.ttest$t.test
          values$ttest.test <- list.ttest$t.test
        } else if ('kruskal' == test) {
          values$kruskal <- kruskal.test(dataset[[dataTable]], rdvs(), rbetween())
          values$kruskal.test <- get.kruskal.table(values$kruskal)
          values$pwc <- kruskal.pwc(dataset[[dataTable]], rdvs(), rbetween(), p.adjust.method = input$p.adjust.method)
          values$pair.wise <- get.kruskal.pwc.table(values$pwc)
        } else if ('srh' == test) {
          values$srh <- scheirer.test(dataset[[dataTable]], rdvs(), rbetween())
          values$srh.test <- get.scheirer.table(values$srh)
          values$pwc <- scheirer.pwc(dataset[[dataTable]], rdvs(), rbetween(), p.adjust.method = input$p.adjust.method)
          values$pair.wise <- get.scheirer.pwc.table(values$pwc)
        } else if ('ancova' == test) {
          values$ancova <- ancova.test(dataset[[dataTable]], rdvs(), rbetween(), rcovar(), input$type, input$effect.size)
          values$ancova.test <- get.ancova.table(values$ancova)
          values$pwc <- ancova.pwc(dataset[[dataTable]], rdvs(), rbetween(), rcovar(), input$p.adjust.method)
          values$pair.wise <- get.ancova.pwc.table(values$pwc)
        } else if ('anova' == test) {
          values$anova <- anova.test(dataset[[dataTable]], rdvs(), rbetween(), wid = wid(), type = input$type, effect.size = input$effect.size)
          values$anova.test <- get.anova.table(values$anova)
          values$pwc <- anova.pwc(dataset[[dataTable]], rdvs(), rbetween(), p.adjust.method = input$p.adjust.method)
          values$pair.wise <- get.anova.pwc.table(values$pwc)
        }
      }

      observeEvent(input$performTest, {
        if (!dataset$isSetup) return(NULL)
        updateResult()

        cname1 <- c("var","n","df","statistic","effsize","magnitude","p","p.signif")
        if ('srh' == test)
          cname1 <- c("var", "Effect", "Df", "Sum Sq", "H", "p.value","p.value.signif")
        if (test %in% c('wilcoxon','ttest'))
          cname1 <- c(".y.","group1", "group2", "n1", "n2","statistic", "estimate",
                      "conf.low", "conf.high", "effsize", "magnitude", "p","p.signif")
        if (test %in% c('ancova','anova'))
          cname1 <- c("var", "Effect", "DFn", "DFd", "SSn", "SSd", "F", "p", input$effect.size, "p.signif")
        shiny2TableMD("result", values[[paste0(test,'.test')]], cname1, prefix = ns('result'))


        if (!test %in% c('wilcoxon','ttest')) {
          cname2 <- c("var","group1","group2","n1","n2","estimate","statistic","p","p.adj","p.adj.signif")
          if ('srh' == test ||  test %in% c('ancova','anova'))
            cname2 <- c("var", rbetween(), "group1", "group2", "estimate", "statistic", "p", "p.adj", "p.adj.signif")
          shiny2TableMD("pairwise", values$pair.wise, cname2, pageLength = 50, prefix=ns('pairwise'))
        }

        if (test %in% c('ancova','anova')) {
          if ('anova' == test) {
            df <- get.anova.emmeans.with.ds(values$pwc, dataset[[dataTable]], rdvs(), rbetween())
          } else if ('ancova' == test) {
            df <- get.ancova.emmeans.with.ds(values$pwc, dataset[[dataTable]], rdvs(), rbetween())
          }
          cname3 <- c("var", rbetween(), "n","emmean","mean","conf.low","conf.high","sd","sd.emms","se.emms")
        } else {
          df <- get.descriptives(dataset[[dataTable]], rdvs(), rbetween())
          cname3 <- c("variable",rbetween(),"n","median","mean","min","max","iqr","sd")
        }
        shiny2TableMD("emms.ds.tbl", df, cname3, prefix=ns("emms-descriptive-statistic"))

        # ... update hypothesis parameters
        if (!paste0(test,'Params') %in% names(dataset))
          dataset[[paste0(test,'Params')]] <- list()

        dataset[[paste0(test,'Params')]][["hypothesis"]] <- list()
        dataset[[paste0(test,'Params')]][["hypothesis"]][[opt.name]] <- input[[opt.name]]
        if ('kruskal' == test || 'srh' == test) {
          dataset[[paste0(test,'Params')]][["hypothesis"]][["p.adjust.method"]] <- input$p.adjust.method
        } else if (test == 'ttest') {
          dataset[[paste0(test,'Params')]][["hypothesis"]][["var.equal"]] <- as.logical(input$vareq)
          dataset[[paste0(test,'Params')]][["hypothesis"]][["hedges.correction"]] <- as.logical(input$hedge)
        } else if (test %in% c('ancova','anova')) {
          dataset[[paste0(test,'Params')]][["hypothesis"]][["effect.size"]] <- input$effect.size
          dataset[[paste0(test,'Params')]][["hypothesis"]][["p.adjust.method"]] <- input$p.adjust.method
        }

        dataset[[test]] <- values[[test]]
        dataset$pwc <- values$pwc
        dataset$ds <- df
      })

      # ... displays plots

      observeEvent(input$updatePlot, {
        if (!dataset$isSetup) return(NULL)

        output$pairwisePlotsUI <- renderUI({
          if (!dataset$isSetup) return(NULL)

          dv <- isolate(input$dv)
          ivs <- isolate(rbetween())
          width <- isolate(input$width)
          height <- isolate(input$height)
          addParam <- isolate(input$addParam)
          font.label.size <- isolate(input$font.label.size)
          step.increase <- isolate(input$step.increase)

          dat <- as.data.frame(dataset$dataTable[[dv]])

          # ... update dataset  parameters

          if (!paste0(test,'Params') %in% names(dataset))
            dataset[[paste0(test,'Params')]] <- list()

          if (!'plot' %in% names(dataset[[paste0(test,'Params')]]))
            dataset[[paste0(test,'Params')]][["plot"]] <- list()

          dataset[[paste0(test,'Params')]][["plot"]][[dv]] <- list(
            width = width, height = height, font.label.size = font.label.size,
            addParam = addParam, step.increase = step.increase
          )

          # ... plots results from pairwise

          if ('wilcoxon' == test) {
            verticalLayout(
              renderPlot({ ggPlotWilcoxon(dat, ivs, dv, values$wt[[dv]], addParam, font.label.size) }, width = width, height = height)
            )
          } else if ('ttest' == test) {
            verticalLayout(
              renderPlot({ ggPlotTTest(dat, ivs, dv, values$tt[[dv]], addParam, font.label.size) }, width = width, height = height)
            )
          } else if ('kruskal' == test) {
            plots <- oneWayNonParamFactPlots(dat, dv, ivs, values[[test]][[dv]][["kt"]], values$pwc[[dv]], addParam=addParam
                                             , font.label.size = font.label.size, step.increase = step.increase)
            do.call(verticalLayout, lapply(names(plots), FUN = function(iv) {
              verticalLayout(
                h4(paste0('Plot of "',dv,'" based on "',iv,'"')),
                renderPlot({ plots[[iv]] }, width = width, height = height))
            }))
          } else if ('srh' == test) {
            plots <- list()
            if (length(ivs) == 1)
              plots <- oneWayNonParamFactPlots(dat, dv, ivs, values[[test]][[dv]], values$pwc[[dv]], addParam=addParam,
                                               font.label.size = font.label.size, step.increase = step.increase, type = 'srh')
            else if (length(ivs) == 2)
              plots <- twoWayNonParamFactPlots(dat, dv, ivs, values[[test]][[dv]], values$pwc[[dv]], addParam=addParam,
                                               font.label.size = font.label.size, step.increase = step.increase, type = 'srh')
            else if (length(ivs) == 3)
              plots <- threeWayNonParamFactPlots(dat, dv, ivs, values[[test]][[dv]], values$pwc[[dv]], addParam=addParam,
                                                 font.label.size = font.label.size, step.increase = step.increase, type = 'srh')
            if (length(ivs) == 3) {
              do.call(verticalLayout, lapply(names(plots), FUN = function(iv) {
                do.call(verticalLayout, lapply(names(plots[[iv]]), FUN = function(grpby) {
                  verticalLayout(
                    h4(paste0('Plot of "',dv,'" based on "',iv,'" and grouped by "',grpby,'"', paste0(' (color: ',setdiff(ivs,c(grpby,iv)),')'))),
                    renderPlot({ plots[[iv]][[grpby]] }, width = width, height = height))
                }))
              }))
            } else if (length(ivs) == 2) {
              do.call(verticalLayout, lapply(names(plots), FUN = function(iv) {
                verticalLayout(
                  h4(paste0('Plot of "',dv,'" based on "',iv,'"', paste0(' (color: ',setdiff(ivs,iv),')'))),
                  renderPlot({ plots[[iv]] }, width = width, height = height))
              }))
            } else {
              do.call(verticalLayout, lapply(names(plots), FUN = function(iv) {
                verticalLayout(
                  h4(paste0('Plot of "',dv,'" based on "',iv,'"')),
                  renderPlot({ plots[[iv]] }, width = width, height = height))
              }))
            }
          } else if ('ancova' == test) {
            plots <- list()
            if (length(ivs) == 1)
              plots <- oneWayAncovaPlots(dat, dv, ivs, values$ancova[[dv]], values$pwc[[dv]], addParam = addParam,
                                         font.label.size = font.label.size, step.increase = step.increase)
            else if (length(ivs) == 2)
              plots <- twoWayAncovaPlots(dat, dv, ivs, values$ancova[[dv]], values$pwc[[dv]], addParam = addParam,
                                         font.label.size = font.label.size, step.increase = step.increase)
            if (length(ivs) == 3) {
              do.call(verticalLayout, lapply(names(plots), FUN = function(iv) {
                do.call(verticalLayout, lapply(names(plots[[iv]]), FUN = function(grpby) {
                  verticalLayout(
                    h4(paste0('Plot of "',dv,'" based on "',iv,'" and grouped by "',grpby,'"', paste0(' (color: ',setdiff(ivs,c(grpby,iv)),')'))),
                    renderPlot({ plots[[iv]][[grpby]] }, width = width, height = height))
                }))
              }))
            } else if (length(ivs) == 2) {
              do.call(verticalLayout, lapply(names(plots), FUN = function(iv) {
                verticalLayout(
                  h4(paste0('Plot of "',dv,'" based on "',iv,'"', paste0(' (color: ',setdiff(ivs,iv),')'))),
                  renderPlot({ plots[[iv]] }, width = width, height = height))
              }))
            } else {
              do.call(verticalLayout, lapply(names(plots), FUN = function(iv) {
                verticalLayout(
                  h4(paste0('Plot of "',dv,'" based on "',iv,'"')),
                  renderPlot({ plots[[iv]] }, width = width, height = height))
              }))
            }
          } else if ('anova' == test) {
            plots <- list()
            if (length(ivs) == 1)
              plots <- oneWayAnovaPlots(dat, dv, ivs, values$anova[[dv]], values$pwc[[dv]], addParam=addParam,
                                        font.label.size = font.label.size, step.increase = step.increase)
            else if (length(ivs) == 2)
              plots <- twoWayAnovaPlots(dat, dv, ivs, values$anova[[dv]], values$pwc[[dv]], addParam=addParam,
                                        font.label.size = font.label.size, step.increase = step.increase)
            else if (length(ivs) == 3)
              plots <- threeWayAnovaPlots(dat, dv, ivs, values$anova[[dv]], values$pwc[[dv]], addParam=addParam,
                                          font.label.size = font.label.size, step.increase = step.increase)
            if (length(ivs) == 3) {
              do.call(verticalLayout, lapply(names(plots), FUN = function(iv) {
                do.call(verticalLayout, lapply(names(plots[[iv]]), FUN = function(grpby) {
                  verticalLayout(
                    h4(paste0('Plot of "',dv,'" based on "',iv,'" and grouped by "',grpby,'"', paste0(' (color: ',setdiff(ivs,c(grpby,iv)),')'))),
                    renderPlot({ plots[[iv]][[grpby]] }, width = width, height = height))
                }))
              }))
            } else if (length(ivs) == 2) {
              do.call(verticalLayout, lapply(names(plots), FUN = function(iv) {
                verticalLayout(
                  h4(paste0('Plot of "',dv,'" based on "',iv,'"', paste0(' (color: ',setdiff(ivs,iv),')'))),
                  renderPlot({ plots[[iv]] }, width = width, height = height))
              }))
            } else {
              do.call(verticalLayout, lapply(names(plots), FUN = function(iv) {
                verticalLayout(
                  h4(paste0('Plot of "',dv,'" based on "',iv,'"')),
                  renderPlot({ plots[[iv]] }, width = width, height = height))
              }))
            }
          }

        })
      })

    }
  )
}
