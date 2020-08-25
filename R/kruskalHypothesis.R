#' @import shiny
kruskalHypothesisUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('kruskalHypothesis')

  mchoices <- list("Wilcoxon's test"="wilcoxon", "Dunn's test"="dunn")
  pchoices <- c("bonferroni", "hommel", "holm", "hochberg")
  addchoices <- list("todos" = "jitter", "média" = "mean", "não" = "none")

  verticalLayout(
    fixedRow(
      column(width = 6, verticalLayout(HTML(""))),
      column(width = 3, radioButtons(ns("pwc.method"), tl("Pairwise comparison method"), choices=mchoices, inline=F))
    ),
    fixedRow(
      column(width = 2, actionButton(ns("performTest"), tl("Perform/Update Test"), icon = icon("running"))),
      column(width = 6, h4(tl("Results from Kruskal-Wallis test"))),
    ),
    df2TableUI(ns("result")), br(),
    h4(tl("Pairwise Comparisons")), br(),
    radioButtons(ns("p.adjust.method"), tl("P-value ajust method"), choices = pchoices, selected = pchoices[1], inline = T, width = "100%"),
    df2TableUI(ns("pairwise")), br(),
    h4(tl("Descriptive Statistics")), df2TableUI(ns("dstbl")), br(), hr(),
    radioButtons(ns("dv"), tl("Y-axis variable"), choices = c("dv"), inline = T, width = "100%"),
    fixedRow(
      column(width = 3, radioButtons(ns("addParam"),  "point style", inline = T, choices = addchoices)),
      column(width = 2, numericInput(ns("width"), "width", value = 800, min = 100, step = 50)),
      column(width = 2, numericInput(ns("height"), "height", value = 600, min = 100, step = 50)),
      column(width = 2, numericInput(ns("font.label.size"), tl("Font text size"), value = 10, min = 4, step = 2)),
      column(width = 2, numericInput(ns("step.increase"), tl("Step of signif."), value = 0.005, min = 0.0001, max = 0.2, step = 0.005)),
      column(width = 1, actionButton(ns("updatePlot"), tl("Update Plot")))
    ),
    uiOutput(ns("pairwisePlotsUI"))
  )
}

kruskalHypothesisMD <- function(id, dataset, dvs = "dvs", between = "between") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('kruskalHypothesis')

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

      # .. update Kruskal-Wallis results

      updateResult <- function() {
        if (dataset$isSetup) {
          values$kruskal <- get.kruskal.test(dataset$dataTable, rdvs(), rbetween(), dv.var = 'var')
          values$kruskal.test <- get.kruskal.table(values$kruskal)
          values$pwc <- get.kruskal.pwc(dataset$dataTable, rdvs(), rbetween(), p.adjust.method = input$p.adjust.method, dv.var = 'var')
          values$pair.wise <- get.kruskal.pwc.table(values$pwc)
        }
      }

      observeEvent(input$performTest, {
        if (!dataset$isSetup) return(NULL)
        if (dataset$isSetup) {
          updateResult()

          cname1 <- c("var","n","df","statistic","effsize","magnitude","p","p.signif")
          df2TableMD("result", values$kruskal.test, cname1, prefix = ns('result'))

          cname2 <- c("var","group1","group2","n1","n2","estimate","statistic","p","p.adj","p.adj.signif")
          df2TableMD("pairwise", values$pair.wise, cname2, pageLength = 50, prefix=ns('pairwise'))

          df.kruskal <- descriptive_statistics(dataset$dataTable,rdvs(),rbetween(),dv.var ='var')
          cname3 <- c("variable",rbetween(),"n","median","mean","min","max","iqr","sd")
          df2TableMD("dstbl", df.kruskal, cname3, prefix=ns("ds"))

          # ... update dataset kruskal parameters
          if (!'kruskalParams' %in% names(dataset)) dataset$kruskalParams <- list()
          dataset$kruskalParams[["hypothesis"]] <- list(
            pwc.method = input$pwc.method,
            p.adjust.method = input$p.adjust.method
          )
          dataset$kruskal <- values$kruskal
          dataset$pwc <- values$pwc
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


          dat <- as.data.frame(dataset$dataTable[which(dataset$dataTable[["var"]] == dv),])

          # ... update dataset kruskal parameters
          if (!'kruskalParams' %in% names(dataset)) dataset$kruskalParams <- list()
          if (!'plot' %in% names(dataset$kruskalParams)) dataset$kruskalParams[["plot"]] <- list()
          dataset$kruskalParams[["plot"]][[dv]] <- list(
            width = width, height = height, font.label.size = font.label.size,
            addParam = addParam, step.increase = step.increase
          )

          # ... plots Kruskal-Wallis results from pairwise
          plots <- oneWayNonParamFactPlots(dat, dv, ivs, values$kruskal[[dv]][["kt"]], values$pwc[[dv]], addParam=addParam,
                                      font.label.size = font.label.size, step.increase = step.increase)

            do.call(verticalLayout, lapply(names(plots), FUN = function(iv) {
              verticalLayout(
                h4(paste0('Plot of "',dv,'" based on "',iv,'"')),
                renderPlot({ plots[[iv]] }, width = width, height = height))
            }))
        })
      })

    }
  )
}
