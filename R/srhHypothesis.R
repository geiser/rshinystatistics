#' @import shiny
srhHypothesisUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('srhHypothesis')

  mchoices <- list("Wilcoxon's test"="wilcoxon")#, "Dunn's test"="dunn")
  pchoices <- c("bonferroni", "hommel", "holm", "hochberg")
  addchoices <- list("todos" = "jitter", "média" = "mean", "não" = "none")

  verticalLayout(
    fixedRow(
      column(width = 6, verticalLayout(HTML(""))),
      column(width = 3, radioButtons(ns("pwc.method"), tl("Pairwise comparison method"), choices=mchoices, inline=F))
    ),
    fixedRow(
      column(width = 2, actionButton(ns("performTest"), tl("Perform/Update Test"), icon = icon("running"))),
      column(width = 6, h4(tl("Results from Scheirer-Ray-Hare test"))),
    ),
    df2TableUI(ns("result")), br(),
    h4(tl("Pairwise Comparisons")), br(),
    radioButtons(ns("p.adjust.method"), tl("P-value ajust method"), choices = pchoices, selected = pchoices[1], inline = T, width = "100%"),
    df2TableUI(ns("pairwise")), br(),
    h4(tl("Descriptive Statistics")), df2TableUI(ns("dstbl")), br(), hr(),
    radioButtons(ns("dv"), tl("Y-axis variable"), choices = c("dv"), inline = T, width = "100%"),
    fixedRow(
      column(width = 3, radioButtons(ns("addParam"),  "point style", inline = T, choices = addchoices)),
      column(width = 2, numericInput(ns("width"), "width", value = 700, min = 100, step = 50)),
      column(width = 2, numericInput(ns("height"), "height", value = 700, min = 100, step = 50)),
      column(width = 2, numericInput(ns("font.label.size"), tl("Font text size"), value = 12, min = 4, step = 2)),
      column(width = 2, numericInput(ns("step.increase"), tl("Step of signif."), value = 0.25, min = 0.05, max = 0.95, step = 0.05)),
      column(width = 1, actionButton(ns("updatePlot"), tl("Update Plot")))
    ),
    uiOutput(ns("pairwisePlotsUI"))
  )
}

srhHypothesisMD <- function(id, dataset, dvs = "dvs", between = "between") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('srhHypothesis')

      wid <- reactiveVal(dataset$variables$wid)
      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      rbetween <- reactiveVal(unique(unlist(dataset$variables[c(between)], use.names = F)))

      observeEvent(dataset$variables, {
        wid(dataset$variables$wid)
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
        rbetween(unique(unlist(dataset$variables[c(between)], use.names = F)))
        updateRadioButtons(session, "dv", choices = rdvs(), selected = rdvs()[1], inline = T)
      })

      values <- reactiveValues()

      # .. update Scheirer-Ray-Hare results

      updateResult <- function() {
        if (dataset$isSetup) {
          values$srh <- get.scheirer.test(dataset$dataTable, rdvs(), rbetween(), dv.var = 'var')
          values$srh.test <- get.scheirer.table(values$srh)
          values$pwc <- get.scheirer.pwc(dataset$dataTable, rdvs(), rbetween(), p.adjust.method = input$p.adjust.method, dv.var = 'var')
          values$pair.wise <- get.scheirer.pwc.table(values$pwc)
        }
      }

      observeEvent(input$performTest, {
        if (!dataset$isSetup) return(NULL)
        if (dataset$isSetup) {
          updateResult()

          cname1 <- c("var", "Effect", "Df", "Sum Sq", "H", "p.value","p.value.signif")
          df2TableMD("result", values$srh.test, cname1, prefix = ns('result'))

          cname2 <- c("var", rbetween(),"group1", "group2","estimate","se","df","statistic","p", "p.adj","p.adj.signif")
          df2TableMD("pairwise", values$pair.wise, cname2, pageLength = 50, prefix=ns('pairwise'))

          df.srh <- descriptive_statistics(dataset$dataTable,rdvs(),rbetween(),dv.var ='var')
          cname3 <- c("variable",rbetween(),"n","median","mean","q1","q3","iqr","sd")
          df2TableMD("dstbl", df.srh, cname3, prefix=ns("ds"))

          # ... update dataset scheirer parameters
          if (!'srhParams' %in% names(dataset)) dataset$srhParams <- list()
          dataset$srhParams[["hypothesis"]] <- list(
            pwc.method = input$pwc.method,
            p.adjust.method = input$p.adjust.method
          )
          dataset$srh <- values$srh
          dataset$pwc <- values$pwc
          dataset$ds <- df.srh
        }
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

          # ... update dataset srh parameters
          if (!'srhParams' %in% names(dataset)) dataset$srhParams <- list()
          if (!'plot' %in% names(dataset$srhParams)) dataset$srhParams[["plot"]] <- list()
          dataset$srhParams[["plot"]][[dv]] <- list(
            width = width, height = height, font.label.size = font.label.size,
            addParam = addParam, step.increase = step.increase
          )

          # ... plots Scheirer-Ray-Hare results from pairwise
          plots <- list()
          if (length(ivs) == 1)
            plots <- oneWayNonParamFactPlots(dat, dv, ivs, values$srh[[dv]], values$pwc[[dv]], addParam=addParam,
                                      font.label.size = font.label.size, step.increase = step.increase, type = 'srh')
          else if (length(ivs) == 2)
            plots <- twoWayNonParamFactPlots(dat, dv, ivs, values$srh[[dv]], values$pwc[[dv]], addParam=addParam,
                                      font.label.size = font.label.size, step.increase = step.increase, type = 'srh')
          else if (length(ivs) == 3)
            plots <- threeWayNonParamFactPlots(dat, dv, ivs, values$srh[[dv]], values$pwc[[dv]], addParam=addParam,
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
        })
      })

    }
  )
}
