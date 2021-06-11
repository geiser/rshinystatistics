#' @import shiny
mdnHypothesisUI <- function(id, test) {
  ns <- NS(id)
  tl <- getTranslator('kruskalHypothesis')

  opt.name <- "pwc.method"
  opt.label <- tl("Pairwise comparison method")
  opt.choices <- list("Wilcoxon's test"="wilcoxon")

  pairwiseCompLayout <- verticalLayout()
  if ('wilcoxon' == test) {
    opt.name <- "alternative"
    opt.label <- tl("Alternative hypothesis")
    opt.choices <- as.list(c('two.sided','greater','less'))
    names(opt.choices) <- c(tl('Two tailed'), tl('Greater than'), tl('Less than'))
  } else {
    pchoices <- c("bonferroni", "hommel", "holm", "hochberg")
    pairwiseCompLayout <- verticalLayout(
      h4(tl("Pairwise Comparisons")), br(),
      radioButtons(ns("p.adjust.method"), tl("P-value ajust method"), choices = pchoices, selected = pchoices[1], inline = T, width = "100%"),
      df2TableUI(ns("pairwise")), br(), hr()
    )
  }

  addchoices <- list("todos" = "jitter", "média" = "mean", "não" = "none")

  verticalLayout(
    fixedRow(
      column(width = 6, verticalLayout(HTML(""))),
      column(width = 3, radioButtons(ns(opt.name), opt.label, choices=opt.choices, inline=F))
    ),
    fixedRow(
      column(width = 2, actionButton(ns("performTest"), tl("Perform/Update Test"), icon = icon("running"))),
      column(width = 6, h4(tl("Results from the hypothesis test"))),
    ),
    df2TableUI(ns("result")), br(), hr(),
    pairwiseCompLayout,
    h4(tl("Descriptive Statistics")), df2TableUI(ns("dstbl")), br(), hr(),
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

mdnHypothesisMD <- function(id, test, dataset, dvs = "dvs", between = "between") {

  opt.name <- "pwc.method"
  if ('wilcoxon' == test) opt.name <- "alternative"

  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('kruskalHypothesis')

      wid <- reactiveVal(dataset$variables$wid)
      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      rbetween <- reactiveVal(unique(unlist(dataset$variables[c(between)], use.names = F)))

      observeEvent(dataset$variables, {
        if (!dataset$isSetup) return(NULL)
        wid(dataset$variables$wid)
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
        rbetween(unique(unlist(dataset$variables[c(between)], use.names = F)))
        updateRadioButtons(session, "dv", choices = rdvs(), selected = rdvs()[1], inline = T)
      })

      values <- reactiveValues()

      # .. update the hypothesis results

      updateResult <- function() {
        if (!dataset$isSetup) return(NULL)
        if ('wilcoxon' == test) {

          list.wtest <- wilcoxon_test(dataset$dataTable, rdvs(), rbetween(), input[[opt.name]], dv.var = 'var', as.list = T)
          values$wt <- list.wtest$wt
          values$ez <- list.wtest$ez
          values$wilcoxon <- list.wtest$wilcoxon.test
          values$wilcoxon.test <- list.wtest$wilcoxon.test

        } else if ('kruskal' == test) {
          values$kruskal <- get.kruskal.test(dataset$dataTable, rdvs(), rbetween(), dv.var = 'var')
          values$kruskal.test <- get.kruskal.table(values$kruskal)
          values$pwc <- get.kruskal.pwc(dataset$dataTable, rdvs(), rbetween(), p.adjust.method = input$p.adjust.method, dv.var = 'var')
          values$pair.wise <- get.kruskal.pwc.table(values$pwc)
        } else if ('srh' == test) {
          values$srh <- get.scheirer.test(dataset$dataTable, rdvs(), rbetween(), dv.var = 'var')
          values$srh.test <- get.scheirer.table(values$srh)
          values$pwc <- get.scheirer.pwc(dataset$dataTable, rdvs(), rbetween(), p.adjust.method = input$p.adjust.method, dv.var = 'var')
          values$pair.wise <- get.scheirer.pwc.table(values$pwc)
        }
      }

      observeEvent(input$performTest, {
        if (!dataset$isSetup) return(NULL)
        updateResult()

        cname1 <- c("var","n","df","statistic","effsize","magnitude","p","p.signif")
        if ('srh' == test)
          cname1 <- c("var", "Effect", "Df", "Sum Sq", "H", "p.value","p.value.signif")
        if ('wilcoxon' == test)
          cname1 <- c(".y.","group1", "group2", "n1", "n2","statistic", "estimate",
                      "conf.low", "conf.high", "effsize", "magnitude", "p","p.signif")
        df2TableMD("result", values[[paste0(test,'.test')]], cname1, prefix = ns('result'))


        if ('wilcoxon' != test) {
          cname2 <- c("var","group1","group2","n1","n2","estimate","statistic","p","p.adj","p.adj.signif")
          if ('srh' == test)
            cname2 <- c("var",rbetween(),"group1","group2","n1","n2","estimate","statistic","p","p.adj","p.adj.signif")
          df2TableMD("pairwise", values$pair.wise, cname2, pageLength = 50, prefix=ns('pairwise'))
        }


        df <- descriptive_statistics(dataset$dataTable, rdvs(), rbetween(), dv.var ='var')
        cname3 <- c("variable",rbetween(),"n","median","mean","min","max","iqr","sd")
        df2TableMD("dstbl", df, cname3, prefix=ns("ds"))

        # ... update hypothesis parameters
        if (!paste0(test,'Params') %in% names(dataset))
          dataset[[paste0(test,'Params')]] <- list()

        dataset[[paste0(test,'Params')]][["hypothesis"]] <- list()
        dataset[[paste0(test,'Params')]][["hypothesis"]][[opt.name]] <- input[[opt.name]]
        if ('kruskal' == test || 'srh' == test) {
          dataset[[paste0(test,'Params')]][["hypothesis"]]$p.adjust.method <- input$p.adjust.method
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
          if (!paste0(test,'Params') %in% names(dataset)) dataset[[paste0(test,'Params')]] <- list()
          if (!'plot' %in% names(dataset[[paste0(test,'Params')]])) dataset[[paste0(test,'Params')]][["plot"]] <- list()
          dataset[[paste0(test,'Params')]][["plot"]][[dv]] <- list(
            width = width, height = height, font.label.size = font.label.size,
            addParam = addParam, step.increase = step.increase
          )

          # ... plots results from pairwise
          if ('wilcoxon' == test) {

            verticalLayout(
              renderPlot({ ggPlotWilcoxon(dat, ivs, dv, values$wt[[dv]], addParam, font.label.size) }, width = width, height = height)
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

          }

        })
      })

    }
  )
}
