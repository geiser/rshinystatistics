#' @import shiny
ancovaHypothesisUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('ancovaHypothesis')

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
      column(width = 6, h4(tl("Results from ANCOVA test"))),
    ),
    h4(tl("Result from ANCOVA")), df2TableUI(ns("result")), br(),
    h4(tl("Pairwise Comparisons")), br(),
    radioButtons(ns("p.adjust.method"), tl("P-value ajust method"), choices = pchoices, selected = pchoices[1], inline = T, width = "100%"),
    df2TableUI(ns("pairwise")), br(),
    h4(tl("Estimated Marginal Means")), df2TableUI(ns("emmeans")), br(), hr(),
    radioButtons(ns("dv"), tl("Y-axis variable"), choices = c("dv"), inline = T, width = "100%"),
    fixedRow(
      column(width = 2, numericInput(ns("width"), "width", value = 700, min = 100, step = 50)),
      column(width = 2, numericInput(ns("height"), "height", value = 700, min = 100, step = 50)),
      column(width = 2, numericInput(ns("font.label.size"), tl("Font text size"), value = 12, min = 4, step = 2)),
      column(width = 2, numericInput(ns("step.increase"), tl("Step of signif."), value = 0.25, min = 0.05, max = 0.95, step = 0.05)),
      column(width = 1, actionButton(ns("updatePlot"), tl("Update plot")))
    ),
    uiOutput(ns("pairwisePlotsUI"))
  )
}


#' @import shiny
ancovaHypothesisMD <- function(id, dataset, dvs = "dvs", between = "between", covar = "covar") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('ancovaHypothesis')

      wid <- reactiveVal(dataset$variables$wid)
      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      rbetween <- reactiveVal(unique(unlist(dataset$variables[c(between)], use.names = F)))
      rcovar <- reactiveVal(unique(unlist(dataset$variables[c(covar)], use.names = F)))

      observeEvent(dataset$variables, {
        wid(dataset$variables$wid)
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
        rbetween(unique(unlist(dataset$variables[c(between)], use.names = F)))
        rcovar(unique(unlist(dataset$variables[c(covar)], use.names = F)))
        updateRadioButtons(session, "dv", choices = rdvs(), selected = rdvs()[1], inline = T)
      })

      values <- reactiveValues()

      # .. update ANOVA results

      updateResult <- function() {
        if (dataset$isSetup) {
          values$aov <- ancova.test(dataset$dataTable, rdvs(), rbetween(), rcovar(),
                                    input$type, input$effect.size, dv.var = 'var')
          values$anova.test <- get.ancova.table(values$aov)
          values$pwc <- ancova.pwc(dataset$dataTable, rdvs(), rbetween(), rcovar(),
                                   input$p.adjust.method, dv.var = 'var')
          values$pair.wise <- get.ancova.pwc.table(values$pwc)
        }
      }

      observeEvent(input$performTest, {
        if (!dataset$isSetup) return(NULL)
        updateResult()

        cname1 <- c("var", "Effect", "DFn", "DFd", "SSn", "SSd", "F", "p", input$effect.size, "p.signif")
        df2TableMD("result", values$anova.test, cname1, prefix = ns('result'))

        cname2 <- c("var", rbetween(), "group1", "group2","estimate","se","df","statistic","p", "p.adj","p.adj.signif")
        df2TableMD("pairwise", values$pair.wise, cname2, pageLength = 50, prefix=ns('pairwise'))

        df.emms <- get.ancova.emmeans.with.ds(values$pwc, dataset$dataTable, rdvs(), rbetween(), dv.var = "var")
        cname3 <- c("var",rbetween(),rcovar(),"n","emmean","se.emms","conf.low","conf.high","mean","median","sd","ci")
        df2TableMD("emmeans", df.emms, cname3, prefix=ns("emmeans"))


        # ... update dataset ancova parameters
        if (!'ancovaParams' %in% names(dataset)) dataset$ancovaParams <- list()
        dataset$ancovaParams[["hypothesis"]] <- list(
          type = input$type,
          effect.size = input$effect.size,
          p.adjust.method = input$p.adjust.method
        )
        dataset$aov <- values$aov
        dataset$pwc <- values$pwc
        dataset$ds <- df.emms
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
          font.label.size <- isolate(input$font.label.size)
          step.increase <- isolate(input$step.increase)


          dat <- as.data.frame(dataset$dataTable[[dv]])

          # ... update dataset ancova parameters
          if (!'ancovaParams' %in% names(dataset)) dataset$ancovaParams <- list()
          if (!'plot' %in% names(dataset$ancovaParams)) dataset$ancovaParams[["plot"]] <- list()
          dataset$ancovaParams[["plot"]][[dv]] <- list(
            width = width, height = height, font.label.size = font.label.size, step.increase = step.increase
          )

          # ... plots ancova results from pairwise
          plots <- list()
          if (length(ivs) == 1)
            plots <- oneWayAncovaPlots(dat, dv, ivs, values$aov[[dv]], values$pwc[[dv]],
                                       font.label.size = font.label.size, step.increase = step.increase)
          else if (length(ivs) == 2)
            plots <- twoWayAncovaPlots(dat, dv, ivs, values$aov[[dv]], values$pwc[[dv]],
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

        })
      })

    }
  )
}
