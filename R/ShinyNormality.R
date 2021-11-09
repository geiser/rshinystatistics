info_for_qq_groups <- function(data, dv, ivs, wid = 'row.pos', dv.var = NULL, skewness = c()) {
  if (is.data.frame(data)) {
    dat <- as.data.frame(data)
    if (!is.null(dv.var))
      dat <- as.data.frame(data[data[[dv.var]] == dv,])
  } else if (is.list(data)) {
    dat <- as.data.frame(data[[dv]])
  }

  for (col in names(skewness))
    dat[[col]] <- dat[[skewness[[col]]]]

  sivs <- unique(ivs[ivs %in% colnames(dat)])

  toReturn <- list()
  freq_df <- subset(rstatix::freq_table(dat, vars = sivs), n >= 3)
  for (i in seq(1,nrow(freq_df))) {
    tbl <- freq_df[i,c(sivs)]
    df <- subset_by_tbl(dat, tbl, group = sivs)
    df <- dplyr::group_by_at(df, dplyr::vars(sivs))

    lbl <- paste0(paste0(tbl,collapse=':'),' (',paste0(names(tbl), collapse=':'),')')

    non.normal <- getNonNormal(df[[dv]], df[[wid]])
    toReturn[[lbl]] <- list(lbl = lbl, data = df, non.normal = non.normal, i = i)
  }
  return(toReturn)
}

#' @import shiny
qqGroupPanel <- function(data, dv, wid = 'row.pos', ids = data[[wid]], name = paste0('All of ', dv),
                         dv.var = NULL, width = 500, height = 400, bins = 30) {
  if (is.data.frame(data)) {
    df <- as.data.frame(data)
    if (!is.null(dv.var))
      df <- df[df[[dv.var]] == dv,]
    df <- df[df[[wid]] %in% ids,]
  } else if (is.list(data)) {
    df <- data[[dv]]
  }

  values <- df[[dv]]
  names(values) <- df[[wid]]

  verticalLayout(
    h4(paste0("Group: ", name))
    , splitLayout(
      cellWidths = width,
      tabsetPanel(
        type = "pills",
        tabPanel(
          paste0("QQ-Classical"),
          renderPlot({
            car::qqPlot(values, ylab = "sample")
          }, width = width, height = height)
        ),
        tabPanel(
          paste0("QQ-Interactive"),
          plotly::renderPlotly({
            plotly::layout(qqPlotly(values), width = width, height = height)
          })
        )
      ),
      tabsetPanel(
        type = "pills",
        tabPanel(
          paste0("Histogram"),
          renderPlot({
            gplot <- ggpubr::gghistogram(
              df, x = dv, y = "..density..", add = "mean",
              bins = bins, palette = "jco", rug = T, add_density = T)
            gplot <- gplot + ggpubr::stat_overlay_normal_density(color = "red", linetype = "dashed")
            gplot
          }, width = width, height = height)
        ),
        tabPanel(paste0("Skewness & Kurtosis"), renderPrint({ symmetry.test(values) }))
      )
    )
  )
}

#' @import shiny
qqResidualPanel <- function(data, dv, wid = 'row.pos', between = c(), within = c(),
                            covar = NULL, dv.var = NULL, width = 500, height = 400, bins = 30) {
  dat <- as.data.frame(data)
  ivs <- c(between, within)
  if (!is.null(dv.var))
    dat <- data[data[[dv.var]] == dv,]

  sformula <- as_formula(dv, between, within, covar, wid, as.character = T)
  if (length(ivs) == 0 && length(covar) == 0) {
    res <- dat[[dv]]
    names(res) <- dat[[wid]]
  } else if (length(within) > 0) {
    res <- as.data.frame(stats::proj(stats::aov(as.formula(sformula), data = dat))[[3]])$Residuals
    names(res) <- dat[[wid]]
  } else {
    res <- residuals(lm(as.formula(sformula), data = dat))
    names(res) <- dat[[wid]]
  }

  verticalLayout(
    h4(paste0("Residual Model: ", sformula))
    ,splitLayout(
      cellWidths = width,
      tabsetPanel(
        type = "pills",
        tabPanel(
          paste0("QQ-Classical"),
          renderPlot({
            car::qqPlot(res, ylab = "sample")
          }, width = width, height = height)
        ),
        tabPanel(
          paste0("QQ-Interactive"),
          plotly::renderPlotly({
            plotly::layout(qqPlotly(res), width = width, height = height)
          })
        )
      ),
      tabsetPanel(
        type = "pills",
        tabPanel(
          paste0("Histogram"),
          renderPlot({
            gplot <- ggpubr::gghistogram(
              data.frame(wid = names(res), res = res), x = "res", y = "..density..", add = "mean",
              bins = bins, palette = "jco", rug = T, add_density = T)
            gplot <- gplot + ggpubr::stat_overlay_normal_density(color = "red", linetype = "dashed")
            gplot
          }, width = width, height = height)
        ),
        tabPanel(paste0("Skewness & Kurtosis"), renderPrint({ symmetry.test(res) }))
      )
    )
  )
}

#' @import shiny
shinyNormalityUI <- function(id) {

  ns <- NS(id)
  tl <- getTranslator()

  verticalLayout(
    uiOutput(ns("normalityAssessmentInResUI")),
    br(), hr(), br(),
    uiOutput(ns("normalityAssessmentPerGroupsUI"))
  )
}

#' @import shiny
shinyNormalityMD <- function(id, dataset, dvs = "dvs", between = "between", within = "within"
                        , covar = "covar", show.residuals = T, show.groups = T, dataTable="dataTable") {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      tl <- getTranslator()

      tip <- ''
      if (show.residuals)
        tip <- paste('A avaliação de distribuição em grupos é <b>opcional</b> quando é feita',
                     'avaliação de normalidade empregando modelos residuais.',
                     'Nossa recomendação é apenas realizar a avaliação de normalide em grupos',
                     'com observações maiores do que n > 30')
      normalityHelp <- paste(
        'Na tabela de avaliação de distribuição de normalidade, empregamos os códigos:<ul>',
        '<li><b>YES</b> para indicar que a distribuição de normalidade é satisfeita</li>',
        '<li><b>NO</b> quando a hipótese nula é rejeitada (indicando que os dados não são de uma distribuição normal)</li>',
        '<li><b>QQ</b> para indicar que o p-value do teste pode ser ignorado, efetuando apenas a avaliação de normalidade com base nos gráficos QQ</li>',
        '<li><b>--</b> indica que o teste de normalidade pode ser ignorado</li>', '</ul>',
        'Se a amostra é suficientemente grande (maiores de 100 observações), os dados precissam apenas uma distribuição aproximadamente normal.
        Assim o nivel de sig. para rejeitar a hipótese nula foi reduzida para p = 0.01 e em casos com maior de 50 observações, adoptamos
        DAgostino-Pearson test ao invés de Shapiro-Wilk.')

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

      # .. Assessment of normality distribution in the residual model

      output$normalityAssessmentInResUI <- renderUI({
        if (!dataset$isSetup || !show.residuals) return(NULL)
        verticalLayout(
          p(h4("Assessment of normality in the residual model")),
          shiny2TableUI(ns("normalityResTbl")),
          uiOutput(ns("assessmentHelp4ResUI"))
        )
      })

      output$assessmentHelp4ResUI <- renderUI({
        if (!dataset$isSetup || !show.residuals || dataset$checkNormality) return(NULL)
        verticalLayout(
          helpText(HTML(normalityHelp)),
          checkboxInput(ns("showQQPlot4Residual"), tl("Show QQ-plots to assess normality distribution in the residual model"), width = "100%"),
          conditionalPanel(
            condition = "input.showQQPlot4Residual", ns = ns,
            fixedRow(
              column(width = 3, radioButtons(ns('dvRes'), tl("Dependent variable"), choices = rdvs(), selected = rdvs()[1], inline = T)),
              column(width = 3, numericInput(ns("widthRes"), "Width", value = 500, min=100, step = 50)),
              column(width = 3, numericInput(ns("heightRes"), "Height", value = 400, min=100, step = 50)),
              column(width = 3, sliderInput(ns("binsRes"), "Number of bins:", min = 5, max = 100, value = 35))
            ),
            uiOutput(ns("qqPlot4ResidualUI"))
          )
        )
      })

      output$normalityAssessmentPerGroupsUI <- renderUI({
        if (!dataset$isSetup || !show.groups) return(NULL)
        verticalLayout(
          p(h4("Assessment of normality in the groups")),
          shiny2TableUI(ns("normalityPerGroupsTbl")),
          uiOutput(ns("assessmentHelp4GroupsUI"))
        )
      })

      output$assessmentHelp4GroupsUI <- renderUI({
        if (!dataset$isSetup || !show.groups || dataset$checkNormality) return(NULL)
        verticalLayout(
          helpText(HTML(tip)), helpText(HTML(normalityHelp)),
          checkboxInput(ns("showQQPlot4Groups"), tl("Show QQ-plots to assess normality distribution for each group"), width = "100%"),
          conditionalPanel(
            condition = "input.showQQPlot4Groups", ns = ns,
            fixedRow(
              column(width = 3, radioButtons(ns('dvGroups'), tl("Dependent variable"), choices = rdvs(), selected = rdvs()[1], inline = T)),
              column(width = 3, numericInput(ns("widthGroups"), "Width", value = 500, min=100, step = 50)),
              column(width = 3, numericInput(ns("heightGroups"), "Height", value = 400, min=100, step = 50)),
              column(width = 3, sliderInput(ns("binsGroups"), "Number of bins:", min = 5, max = 100, value = 35))
            ),
            uiOutput(ns("qqPlot4GroupsUI"))
          )
        )
      })


      # ... tables of normality assessment

      updateNormalityTables <- function() {
        if (!dataset$isSetup) return(NULL)
        data <- dataset[[dataTable]]
        if (show.residuals) {
          df <- do.call(normality.test.by.residual, list(
            data = data, dvs = rdvs(), between = rbetween(), within = rwithin()
            , covar = rcovar(), wid = wid(), skewness = getSkewnessMap(dataset$skewness)))
          cnames <- c('var','normality','method','statistic','p','p.signif')
          shiny2TableMD("normalityResTbl", df, cnames, prefix = ns('normality-residual-assessment'))
        }

        dargs <- list(data = data, dvs = rdvs(), ivs = c(rbetween(), rwithin())
                      , type = 'mean_sd', normality.test = T, skewness = getSkewnessMap(dataset$skewness))
        df.grp <- do.call(get.descriptives, dargs)
        shiny2TableMD("normalityPerGroupsTbl", df.grp, prefix = ns('normality-assessment-per-group'))
      }

      observeEvent(dataset$isSetup, { if (dataset$isSetup) updateNormalityTables() })
      observeEvent(dataset[[dataTable]], { if (dataset$isSetup) updateNormalityTables() })

      # .. QQ plots for residuals

      output$qqPlot4ResidualUI <- renderUI({
        if (!dataset$isSetup) return(NULL)
        dv <- input$dvRes
        dat <- as.data.frame(dataset[[dataTable]][[dv]])
        within <- rwithin()[rwithin() %in% colnames(dat)]
        between <- rbetween()[rbetween() %in% colnames(dat)]

        skewness = getSkewnessMap(dataset$skewness)
        for (col in names(skewness))
          dat[[col]] <- dat[[skewness[[col]]]]

        sformula <- as_formula(dv, between, within, rcovar(), wid())
        if (length(between) == 0 && length(within) == 0 && length(rcovar()) == 0) {
          res <- dat[[dv]]
          names(res) <- dat[[wid()]]
        } else if (length(within) > 0) {
          res <- as.data.frame(stats::proj(stats::aov(sformula, data = dat))[[3]])$Residuals
          names(res) <- dat[[wid()]]
        } else {
          res <- residuals(lm(sformula, data = dat))
          names(res) <- dat[[wid()]]
        }

        selected <- getNonNormal(res, x.name=names(res), step = 1, plimit = 0.05)
        params <- list(data = dat, wid = wid(), dv = dv, between = between, within = within, covar = rcovar(),
                       width = input$widthRes, height = input$heightRes, bins = input$binsRes)
        verticalLayout(
          do.call(qqResidualPanel, params),
          span(tl("To achive normality, we suggest to remove the elements:")),
          selectInput(ns(paste0('extremeRes',dv,'Input')), '',choices = dat[[wid()]], selected = selected, multiple = T, width = '100%'),
          actionButton(ns(paste0('removeRes',dv,'Button')), tl("Remove to achieve normality")), br()
        )
      })

      lapply(rdvs(), FUN = function(dv){
        observeEvent(input[[paste0('removeRes',dv,'Button')]], {
          if (!dataset$isSetup) return(NULL)
          ids <- isolate(dataset$addToRemoveForNormality[[dv]])
          dataset$addToRemoveForNormality[[dv]] <- c(ids, input[[paste0('extremeRes',dv,'Input')]])
        })
      })

      # .. QQ plots for groups

      infoGroupQQs <- reactiveVal(NULL)

      output$qqPlot4GroupsUI <- renderUI({
        if (!dataset$isSetup) return(NULL)
        dv <- input$dvGroups
        infoGroupQQs(info_for_qq_groups(dataset[[dataTable]], dv, c(rbetween(),rwithin()), wid()
                                        , skewness = getSkewnessMap(dataset$skewness)))
        do.call(verticalLayout, lapply(infoGroupQQs(), FUN = function(info) {
          params <- list(
            data = info$data, dv = dv, wid = wid(), name = info$lbl,
            width = input$widthGroups, height = input$heightGroups, bins = input$binsGroups
          )
          verticalLayout(
            do.call(qqGroupPanel, params),
            span(tl("To achive normality, we suggest to remove the elements:")),
            selectInput(ns(paste0('extremeGroup',dv,'Input',info$i)), '', choices = info$data[[wid()]], selected = info$non.normal, multiple = T, width = '100%'),
            actionButton(ns(paste0('removeGroup',dv,'Button',info$i)), tl("Remove to achieve normality")), br()
          )
        }))
      })

      observeEvent(infoGroupQQs(), {
        lapply(rdvs(), FUN = function(dv) {
          lapply(infoGroupQQs(), FUN = function(info) {
            observeEvent(input[[paste0('removeGroup',dv,'Button',info$i)]], {
              if (!dataset$isSetup) return(NULL)
              ids <- isolate(dataset$addToRemoveForNormality[[dv]])
              dataset$addToRemoveForNormality[[dv]] <- c(ids, input[[paste0('extremeGroup',dv,'Input',info$i)]])
            })
          })
        })
      })

    }
  )
}
