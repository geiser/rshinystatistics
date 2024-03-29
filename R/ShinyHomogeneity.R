#' @import shiny
shinyHomogeneityUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator()
  homogeHelp <- paste(
    'Na tabela de avaliação da homogeneidade das variâncias, empregamos testes de Levene,',
    'e  os resultados de significância são apresentados na columna "p.signif" com os valores:<ul>',
    '<li><b>ns</b> para indicar que a a hipótese nula (as varianças das observações provêm de um mesmo grupo amostral) não é rejeitada</li>',
    '<li><b>*</b>, <b>**</b> e <b>***</b> quando a significância é menor do 0.05 indicando que a homogeneidade de variâncias é rejeitada</li>','</ul>',
    'Se a amostra é suficientemente grande (maiores de 100 observações), o nivel de sig. para rejeitar a hipótese nula foi reduzida',
    'para p = 0.01. Se a hipótesis é rejeitada, indicando que as amostras não apresentam homogeneidade nas variânças,',
    'você deve seguir as seguintes recomendações:<ul>',
    '<li>Se está conduzindo um teste t (t-test), você deve empregar o método Welch</li>',
    '<li>Se está conduzindo ANCOVA é melhor empregar ANOVA considerando a covariante como uma variavel between-subject</li>',
    '<li>Se está conduzindo ANOVA, você deve utilizar o método não parametrico equivalente</li></ul>')

  verticalLayout(
    h4(tl("Homogeneity Test")),
    shiny2TableUI(ns("homogeneityTable")), helpText(HTML(homogeHelp))
  )
}

#' @import shiny
shinyHomogeneityMD <- function(id, dataset, dvs = "dvs", between = "between", within = "within", covar = "covar", dataTable = 'dataTable') {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tl <- getTranslator()
    vars <- reactiveValues(
      wid = dataset$variables$wid,
      dvs = unique(unlist(dataset$variables[c(dvs)], use.names = F)),
      between = unique(unlist(dataset$variables[c(between)], use.names = F)),
      within = unique(unlist(dataset$variables[c(within)], use.names = F)),
      covar = unique(unlist(dataset$variables[c(covar)], use.names = F))
    )

    observeEvent(dataset$variables, {
      req(dataset$isSetup)
      vars$wid = dataset$variables$wid
      vars$dvs = unique(unlist(dataset$variables[c(dvs)], use.names = F))
      vars$between = unique(unlist(dataset$variables[c(between)], use.names = F))
      vars$within = unique(unlist(dataset$variables[c(within)], use.names = F))
      vars$covar = unique(unlist(dataset$variables[c(covar)], use.names = F))
    })

    # Homogeneity test
    updateHomogTbl <- function() {
      req(dataset$isSetup)
      h.test <- homogeneity.test(
        dataset[[dataTable]], vars$dvs, vars$between, vars$within, vars$covar,
        dv.var = 'var', skewness = getSkewnessMap(dataset$skewness))
      shiny2TableMD("homogeneityTable", h.test, prefix = ns(''))
    }

    observeEvent(dataset$isSetup, { updateHomogTbl() })
    observeEvent(dataset[[dataTable]], { updateHomogTbl() })

  })
}
