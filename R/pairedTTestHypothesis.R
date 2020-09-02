#' @import shiny
pairedTTestHypothesisUI <- function(id) {
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
A média dos grupos são aproximadamente iguais.
</li>
<li> Hipótese alternativa - Bicaudal
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{alt}\" title=\"H_{alt}\" />):
A média dos grupos são significativamente diferentes.
</li>
<li> Hipótese alternativa - Maior que
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{alt}\" title=\"H_{alt}\" />):
A média do grupo 1 é significativamente maior do que a média do grupo 2.
</li>
<li> Hipótese alternativa - Menor que
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{alt}\" title=\"H_{alt}\" />):
A média do grupo 1 é significativamente menor do que a média do grupo 2.
</li>
</ul>
</p>
<br/>
<p>
Observações:
<ul>
<li>O parâmetro de interesse é a diferença entre duas médias, <img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;u_{1}&space;-&space;u_{2}\" title=\"u_{1} - u_{2}\" /></li>
<li>t-statistic é <img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;s&space;=&space;\\frac{(\\overline{x}_{1}&space;-&space;\\overline{x}_{2})-(u_{1}&space;-&space;u_{2})}{\\sqrt{\\frac{s_{1}^{2}}{n_{1}}&space;&plus;&space;\\frac{s_{2}^{2}}{n_{2}}}}\\\" title=\"statistic = \\frac{(\\overline{x}_{1} - \\overline{x}_{2})-(u_{1} - u_{2})}{\\sqrt{\\frac{s_{1}^{2}}{n_{1}} + \\frac{s_{2}^{2}}{n_{2}}}}\" /></li>
</ul>
</p>'
  ns <- NS(id)
  tl <- getTranslator('pairedTTestHypothesis')

  alt.choices <- list("Bicaudal" = "two.sided", "Maior que" = "greater", "Menor que" = "less")
  method.choices <- list("Welch's t-test" = FALSE, "Student's t-test" = TRUE)
  eff.choices <- list("Não" = FALSE, "Sim" = TRUE)
  add.choices <- list("todos" = "jitter", "média" = "mean", "não" = "none")
  verticalLayout(
    fixedRow(
      column(
        width = 12,
        fixedRow(
          column(width = 6, HTML(tTestHTML)),
          column(width = 2, radioButtons(ns("alternative"), tl("Direção do teste"), inline = F, choices = alt.choices)),
          column(width = 2, radioButtons(ns("var.equal"), "Método", inline = F, choices = method.choices)),
          column( width = 2, radioButtons(ns("hedges.correction"), "Hedges' effsize", inline = F, choices = eff.choices))
        )
      )
    ),
    fixedRow(
      column(width = 2, actionButton(ns("performTest"), tl("Perform/Update Test"), icon = icon("running"))),
      column(width = 8, h4(tl("Result from independent sample t-Test"))),
    ),
    df2TableUI(ns("result")), br(), hr(),
    h4(tl("Descriptive Statistics")), df2TableUI(ns("dstbl")), br(), hr(),
    radioButtons(ns("dv"), tl("Y-axis variable"), choices = c("dv"), inline = T, width = "100%"),
    fixedRow(
      column(width = 3, br()),
      column(width = 2, numericInput(ns("width"), "width", value = 800, min = 100, step = 50)),
      column(width = 2, numericInput(ns("height"), "height", value = 600, min = 100, step = 50)),
      column(width = 2, numericInput(ns("font.label.size"), tl("Font text size"), value = 10, min = 4, step = 2)),
      column(width = 1, actionButton(ns("updatePlot"), tl("Update Plot")))
    ),
    uiOutput(ns("pairedTTestPlotsUI"))
  )
}


#' @import shiny
pairedTTestHypothesisMD <- function(id, dataset, dvs = "dvs", iv = "iv") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('pairedTTestHypothesis')

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

      # .. update t-test results

      updateResult <- function() {
        if (dataset$isSetup) {
          list.ttest <- paired_ttest(
            dataset$dataTable, rdvs(), riv(), input$alternative, as.logical(input$var.equal),
            as.logical(input$hedges.correction), dv.var = 'var', as.list = T)

          values$tt <- list.ttest$tt
          values$ez <- list.ttest$ez
          values$t.test <- list.ttest$t.test
        }
      }

      observeEvent(input$performTest, {
        if (!dataset$isSetup) return(NULL)
        updateResult()
        cnames <- c(".y.", "group1", "group2", "n1", "n2","statistic", "df", "p", "estimate",
                    "conf.low", "conf.high", "effsize", "magnitude", "p.signif")
        df2TableMD("result", values$t.test, cnames, prefix = ns('result'))

        ttdf <- descriptive_statistics(dataset$dataTable, rdvs(),riv(),dv.var ='var')
        cname2 <- c("variable",riv(),"n","mean","median","min","max","sd","iqr")
        df2TableMD("dstbl", ttdf, cname2, prefix=ns("ds"))

        # ... update dataset independent sample t-test parameters
        if (!'pairedTTestParams' %in% names(dataset)) dataset$pairedTTestParams <- list()
        dataset$pairedTTestParams[["hypothesis"]] <- list(
          alternative = input$alternative,
          var.equal = as.logical(input$var.equal),
          hedges.correction = as.logical(input$hedges.correction)
        )
        dataset$t.test <- values$t.test
      })

      # ... displays plots

      observeEvent(input$updatePlot, {
        if (!dataset$isSetup) return(NULL)
        output$pairedTTestPlotsUI <- renderUI({
          if (!dataset$isSetup) return(NULL)
          dv <- isolate(input$dv)
          dat <- as.data.frame(dataset$dataTable[[dv]])

          id <- isolate(wid())
          iv <- intersect(isolate(riv()), colnames(dat))

          width <- isolate(input$width)
          height <- isolate(input$height)
          font.label.size <- isolate(input$font.label.size)



          # ... update dataset for independent sample t-test parameters
          if (!'pairedTTestParams' %in% names(dataset)) dataset$pairedTTestParams <- list()
          if (!'plot' %in% names(dataset$pairedTTestParams)) dataset$pairedTTestParams[["plot"]] <- list()
          dataset$pairedTTestParams[["plot"]][[dv]] <- list(
            width = width, height = height, font.label.size = font.label.size
          )

          # ... plots independent sample t-test
          ttplot <- ggPlotPairedTTest(dat, iv, dv, values$tt[[dv]], id, font.label.size)
          verticalLayout(
            renderPlot({ ttplot }, width = width, height = height)
          )
        })
      })

    }
  )
}
