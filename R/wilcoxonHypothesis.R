#' @import shiny
wilcoxonHypothesisUI <- function(id) {
  tTestHTML <- '
<br/>
<p>
O objetivo do teste de hipótese é determinar a probabilidade (<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;0&space;\\leq&space;p&space;\\leq&space;1\" title=\"0 \\leq p \\leq 1\" />)
dos resultados observados, pressupondo que a hipótese nula seja verdadeira.
</p>
<p>
<ul>
<li> Hipótese nula
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{null}\" title=\"H_{null}\" />):
A mediana (median) dos grupos são aproximadamente iguais.
</li>
<li> Hipótese alternativa - Bicaudal
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{alt}\" title=\"H_{alt}\" />):
A mediana dos grupos são significativamente diferentes.
</li>
<li> Hipótese alternativa - Maior que
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{alt}\" title=\"H_{alt}\" />):
A mediana do grupo 1 é significativamente maior do que a mediana do grupo 2.
</li>
<li> Hipótese alternativa - Menor que
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{alt}\" title=\"H_{alt}\" />):
A mediana do grupo 1 é significativamente menor do que a mediana do grupo 2.
</li>
</ul>
</p>
<br/>
'
  ns <- NS(id)
  tl <- getTranslator('wilcoxonHypothesis')

  alt.choices <- list("Bicaudal" = "two.sided", "Maior que" = "greater", "Menor que" = "less")
  add.choices <- list("todos" = "jitter", "mediana" = "median", "não" = "none")
  verticalLayout(
    fixedRow(
      column(
        width = 12,
        fixedRow(
          column(width = 6, HTML(tTestHTML))
          , column(width = 2, radioButtons(ns("alternative"), tl("Direção do teste"), inline = F, choices = alt.choices))
        )
      )
    ),
    fixedRow(
      column(width = 2, actionButton(ns("performTest"), tl("Perform/Update Test"), icon = icon("running"))),
      column(width = 8, h4(tl("Result from independent sample wilcoxon test"))),
    ),
    df2TableUI(ns("result")), br(), hr(),
    h4(tl("Descriptive Statistics")), df2TableUI(ns("dstbl")), br(), hr(),
    radioButtons(ns("dv"), tl("Y-axis variable"), choices = c("dv"), inline = T, width = "100%"),
    fixedRow(
      column(width = 3, radioButtons(ns("addParam"),  "point style", inline = T, choices = add.choices)),
      column(width = 2, numericInput(ns("width"), "width", value = 700, min = 100, step = 50)),
      column(width = 2, numericInput(ns("height"), "height", value = 700, min = 100, step = 50)),
      column(width = 2, numericInput(ns("font.label.size"), tl("Font text size"), value = 12, min = 4, step = 2)),
      column(width = 1, actionButton(ns("updatePlot"), tl("Update Plot")))
    ),
    uiOutput(ns("wilcoxonPlotsUI"))
  )
}


#' @import shiny
wilcoxonHypothesisMD <- function(id, dataset, dvs = "dvs", iv = "iv") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('wilcoxonHypothesis')

      wid <- reactiveVal(dataset$variables$wid)
      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      riv <- reactiveVal(unique(unlist(dataset$variables[c(iv)], use.names = F)))

      observeEvent(dataset$variables, {
        wid(dataset$variables$wid)
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
        riv(unique(unlist(dataset$variables[c(iv)], use.names = F)))
        updateRadioButtons(session, "dv", choices = rdvs(), selected = rdvs()[1], inline = T)
      })

      values <- reactiveValues()

      # .. update wilcoxon-test results

      updateResult <- function() {
        if (dataset$isSetup) {
          list.wtest <- wilcoxon_test(
            dataset$dataTable, rdvs(), riv(), input$alternative, dv.var = 'var', as.list = T)

          values$wt <- list.wtest$wt
          values$ez <- list.wtest$ez
          values$wilcoxon.test <- list.wtest$wilcoxon.test
        }
      }

      observeEvent(input$performTest, {
        if (!dataset$isSetup) return(NULL)
        updateResult()

        cnames <- c(".y.", "group1", "group2", "n1", "n2","statistic", "estimate",
                    "conf.low", "conf.high", "effsize", "magnitude", "p","p.signif")
        df2TableMD("result", values$wilcoxon.test, cnames, prefix = ns('result'))

        df.wlx <- descriptive_statistics(dataset$dataTable, rdvs(),riv(),dv.var ='var')
        cname2 <- c("variable",riv(),"n","median","mean","min","max","iqr","sd")
        df2TableMD("dstbl", df.wlx, cname2, prefix=ns("ds"))


        # ... update dataset independent sample wilcoxon-test parameters
        if (!'wilcoxonParams' %in% names(dataset)) dataset$wilcoxonParams <- list()
        dataset$wilcoxonParams[["hypothesis"]] <- list(
          alternative = input$alternative
        )
        dataset$wilcoxon.test <- values$wilcoxon.test
        dataset$ds <- df.wlx
      })

      # ... displays plots

      observeEvent(input$updatePlot, {
        if (!dataset$isSetup) return(NULL)
        output$wilcoxonPlotsUI <- renderUI({
          if (!dataset$isSetup) return(NULL)
          iv <- isolate(riv())
          dv <- isolate(input$dv)
          width <- isolate(input$width)
          height <- isolate(input$height)
          addParam <- isolate(input$addParam)
          font.label.size <- isolate(input$font.label.size)

          dat <- as.data.frame(dataset$dataTable[[dv]])

          # ... update dataset for independent sample wilcoxon-test parameters
          if (!'wilcoxonParams' %in% names(dataset)) dataset$wilcoxonParams <- list()
          if (!'plot' %in% names(dataset$wilcoxonParams)) dataset$wilcoxonParams[["plot"]] <- list()
          dataset$wilcoxonParams[["plot"]][[dv]] <- list(
            width = width, height = height, font.label.size = font.label.size, addParam = addParam
          )

          # ... plots wilcoxon-test
          wtplot <- ggPlotWilcoxon(dat, iv, dv, values$wt[[dv]], addParam, font.label.size)
          verticalLayout(
            renderPlot({ wtplot }, width = width, height = height)
          )
        })
      })

    }
  )
}
