#' @import shiny
normalityUI <- function(id) {
  normalityHTML <- "
<p>Para efetuar testes paramétricos, se a amostra é suficientemente grande (maiores de 30 observações per grupo),
a distribuição da média precissa apenas ter uma distribuição <i>aproximadamente</i> normal (nivel de sig. <i>p = 0.01</i>).
Em amostras de tamanho pequeno (menos de 30 observações per grupo), a normalidade deve ser satisfeita em todos os grupos.
</p>
<br/>
<p>
Em casos com <i>maior do que 50</i> observações, adoptamos D'Agostino-Pearson test.
Para tamanhos de amostra grande de (mais dos 100 observações), adoptamos o nível de sig. de <i>p = 0.001</i>,
os testes de normalidade podem ser ignorados baseando a decissão apenas nos graficos QQ-plots e histogramas.
Finalmente, para casos com amostras maiores de 200 observações, o teste de normalidade pode ser ignorado com base no teorema do límite central.
</p>
<ul>
<li> Hipótese nula
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{null}\" title=\"H_{null}\" />):
Os dados são provenientes de uma distribuição normal.
</li>
<li> Hipótese alternativa
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{alt}\" title=\"H_{alt}\" />):
Os dados são provenientes de uma distribuição não normal.
</li>
</ul>
"
  ns <- NS(id)
  tl <- getTranslator("normality")

  verticalLayout(
    br(), p(strong(tl("Normality test"))), br(),
    div(HTML(normalityHTML)), br(), br(),
    p(strong(tl("Assess normality of residuals"))),
    df2TableUI(ns("normalityResTbl")), br(),
    uiOutput(ns("normalityPerGroupsUI")), br(),
    checkboxInput(ns("showQQPlot"), tl("Show QQ and histograms with density plots"), width = "100%"),
    conditionalPanel(
      condition = "input.showQQPlot", ns = ns,
      fixedRow(
        column(width = 3, numericInput(ns("width"), "Width", value = 500, min=100, step = 50)),
        column(width = 3, numericInput(ns("height"), "Height", value = 400, min=100, step = 50)),
        column(width = 3, sliderInput(ns("bins"), "Number of bins:", min = 5, max = 100, value = 35))
      ),
      radioButtons(ns('dv'), tl("Dependent variable"), choices = c("dv"), inline = T),
      uiOutput(ns("qqResidualPlotUI")))
    , uiOutput(ns("qqGroupPlotUI"))
  )
}

#' @import shiny
normalityMD <- function(id, dataset, dvs = "dvs", between = "between", within = "within", covar = "covar", only.residuals = F, table="dataTable") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('normality')

      wid <- reactiveVal(dataset$variables$wid)
      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      rbetween <- reactiveVal(unique(unlist(dataset$variables[c(between)], use.names = F)))
      rwithin <- reactiveVal(unique(unlist(dataset$variables[c(within)], use.names = F)))
      rcovar <- reactiveVal(unique(unlist(dataset$variables[c(covar)], use.names = F)))
      updateRadioButtons(session, "dv", choices = rdvs(), selected = rdvs()[1], inline = T)

      observeEvent(dataset$variables, {
        wid(dataset$variables$wid)
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
        rbetween(unique(unlist(dataset$variables[c(between)], use.names = F)))
        rwithin(unique(unlist(dataset$variables[c(within)], use.names = F)))
        rcovar(unique(unlist(dataset$variables[c(covar)], use.names = F)))
        updateRadioButtons(session, "dv", choices = rdvs(), selected = rdvs()[1], inline = T)
      })

      # ... tables of normality assessment

      output$normalityPerGroupsUI <- renderUI({
        if (dataset$isSetup && !only.residuals) {
          verticalLayout(
            p(strong(tl("Assess normality for all groups"))),
            df2TableUI(ns("normalityGroupTbl")), br()
          )
        }
      })

      updateNormalityTables <- function() {
        if (dataset$isSetup) {
          df <- do.call(normality_test_by_res, list(
            data = dataset[[table]], dvs = rdvs(), between = rbetween(), within = rwithin(),
            covar = rcovar(), wid = wid(), dv.var = 'var'))
          df2TableMD("normalityResTbl", df, prefix = ns('residual'))
          if (!only.residuals) {
            df.grp <- normality_test_per_group(dataset[[table]], rdvs(), c(rbetween(),rwithin()), dv.var = 'var')
            cnames <- c('var',rbetween(),rwithin(),'n','skewness','kurtosis','symmetry','statistic','p','p.signif','normality')
            df2TableMD("normalityGroupTbl", df.grp, cnames, prefix = ns('per-groups'))
          }
        }
      }

      observeEvent(dataset$isSetup, {
        if (dataset$isSetup) updateNormalityTables()
      })

      observeEvent(dataset[[table]], {
        if (dataset$isSetup) updateNormalityTables()
      })

      # ... QQ and histogram with density plots

      output$qqResidualPlotUI <- renderUI({
        if (dataset$isSetup) {
          dat <- as.data.frame(dataset[[table]][[input$dv]])
          within <- rwithin()[rwithin() %in% colnames(dat)]
          between <- rbetween()[rbetween() %in% colnames(dat)]

          sformula <- as_formula(input$dv, between, within, rcovar(), wid())
          if (length(between) == 0 && length(within) == 0 && length(rcovar()) == 0) {
            res <- dat[[input$dv]]
            names(res) <- dat[[wid()]]
          } else if (length(within) > 0) {
            res <- as.data.frame(stats::proj(stats::aov(sformula, data = dat))[[3]])$Residuals
            names(res) <- dat[[wid()]]
          } else {
            res <- residuals(lm(sformula, data = dat))
            names(res) <- dat[[wid()]]
          }

          selected <- getNonNormal(res, x.name=names(res), step = 2, plimit = 0.05)
          params <- list(
            data = dataset[[table]][[input$dv]], wid = wid(), dv = input$dv,
            between = between, within = within, covar = rcovar(),
            width = input$width, height = input$height, bins = input$bins)

          verticalLayout(
            do.call(qqResidualPanel, params),
            span(tl("To achive normality, we suggest to remove the elements:")),
            selectInput(ns(paste0('extremeRes',input$dv,'Input')), '',
                        choices = dat[[wid()]], selected = selected, multiple = T, width = '100%'),
            actionButton(ns(paste0('removeRes',input$dv,'Button')), tl("Remove to achieve normality"))
          )
        }
      })

      lapply(rdvs(), FUN = function(dv){
        observeEvent(input[[paste0('removeRes',input$dv,'Button')]], {
          if (!dataset$isSetup) return(NULL)
          ids <- isolate(dataset$addToRemoveForNormality[[input$dv]])
          dataset$addToRemoveForNormality[[input$dv]] <- c(ids, input[[paste0('extremeRes',input$dv,'Input')]])
        })
      })


      infoGroupQQs <- reactiveVal(NULL)

      output$qqGroupPlotUI <- renderUI({
        if (!dataset$isSetup) return(NULL)
        if (!input$showQQPlot || only.residuals) br()
        else if (input$showQQPlot)  {
          if (!only.residuals) {
            infoGroupQQs(
              info_for_qq_groups(dataset[[table]], input$dv, c(rbetween(),rwithin()), wid(), dv.var = 'var')
            )
          }
          do.call(verticalLayout, lapply(infoGroupQQs(), FUN = function(info) {
            params <- list(
              data = info$data, dv = input$dv, wid = wid(), name = info$lbl,
              width = input$width, height = input$height, bins = input$bins
            )
            verticalLayout(
              br(), do.call(qqGroupPanel, params),
              span(tl("To achive normality, we suggest to remove the elements:")),
              selectInput(ns(paste0('extremeGroup',input$dv,'Input',info$i)), '', choices = info$data[[wid()]], selected = info$non.normal, multiple = T, width = '100%'),
              actionButton(ns(paste0('removeGroup',input$dv,'Button',info$i)), tl("Remove to achieve normality"))
            )
          }))
        }
      })


      if (!only.residuals) {
        observeEvent(infoGroupQQs(), {
          lapply(rdvs(), FUN = function(dv){
            lapply(infoGroupQQs(), FUN = function(info) {
              observeEvent(input[[paste0('removeGroup',input$dv,'Button',info$i)]], {
                if (!dataset$isSetup) return(NULL)
                ids <- isolate(dataset$addToRemoveForNormality[[input$dv]])
                dataset$addToRemoveForNormality[[input$dv]] <- c(ids, input[[paste0('extremeGroup',input$dv,'Input',info$i)]])
              })
            })
          })
        })
      }

    }
  )
}
