#' @import shiny
homogeneityUI <- function(id) {
  homogeHTML <- "
<p>O teste de Levene é utilizado para avaliar a igualdade de variâncias em dois ou mais grupos.
Assim o Levene's test, avalia a homogeidade das varianças empregando:
</p>
<ul>
<li> Hipótese nula
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{75}&space;H_{null}\" title=\"H_{null}\" />):
As varianças das amostras dos diferentes grupos são iguais.
</li>
<li> Hipótese alternativa
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{75}&space;H_{alt}\" title=\"H_{alt}\" />):
As varianças das amostras dos diferentes grupos são diferentes.
</li>
</ul>
<br/>
<p>
Procedimentos estatísticos parâmetricos precissam de igualdade de variações das amostras. Rejeição, da hipótese nula
<i>p.value</i> com valores menores do que <i>0.05</i> (0 **** 0.0001 *** 0.001 ** 0.01 * 0.05) indicam que os dados
das amotras provêm de grupos de populações não idênticas (grupos de individuos diferentes).
</p>
<br/>
<p>
Para <i>n maior do que 30</i> adoptamos o nível de significance <i>p = 0.01</i>, e para <i>n maior do que 100</i>
adoptamos o nível de signif. de <i>p = 0.001</i>
</p>
<br/>
<p>
Se considera que as amostras provêm de grupos idênticos, você pode efetuar o teste paramêtrico.
<ul>
<li>Se está conduzindo um teste t (t-test), você deverá usar sempre o método Welch's test.</li>
<li>Se está conduzindo ANCOVA é melhor empregar ANOVA considerando a covariante como variavel between-subject</li>
<li>Se está conduzindo ANOVA, você pode utilizar o método robusto baseado em Wilcox's (ainda não disponivel no rshiny-statistics).</li>
</ul>
<i>Caso nemhuma das condições sejam satisfeitas, e as amostras são dos mesmos grupos é melhor usar testes não parâmetrico</i>
</p>"

  ns <- NS(id)
  tl <- getTranslator('homogeneity')

  verticalLayout(
    h4(tl("Homogeneity Test")),
    HTML(homogeHTML),
    br(),
    fixedRow(
      column(width = 2, actionButton(ns("performTest"), tl("Perform/Update Test"), icon = icon("running"))),
      column(width = 6, h4(tl("Asessing homogeneity")))
    ),
    df2TableUI(ns("homogeneityTable"))
  )
}

#' @import shiny
homogeneityMD <- function(id, dataset, dvs = "dvs", between = "between", within = "within", covar = "covar") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('homogeneity')

      wid <- reactiveVal(dataset$variables$wid)
      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      rbetween <- reactiveVal(unique(unlist(dataset$variables[c(between)], use.names = F)))
      rwithin <- reactiveVal(unique(unlist(dataset$variables[c(within)], use.names = F)))
      rcovar <- reactiveVal(unique(unlist(dataset$variables[c(covar)], use.names = F)))

      observeEvent(dataset$variables, {
        wid(dataset$variables$wid)
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
        rbetween(unique(unlist(dataset$variables[c(between)], use.names = F)))
        rwithin(unique(unlist(dataset$variables[c(within)], use.names = F)))
        rcovar(unique(unlist(dataset$variables[c(covar)], use.names = F)))
      })

      # Homogeneity test

      observeEvent(input$performTest, {
        if (!dataset$isSetup) return(NULL)
        h.test <- homogeneity_test(dataset$dataTable, rdvs(), rbetween(), rwithin(), rcovar(), dv.var = 'var')
        df2TableMD("homogeneityTable", h.test, prefix = ns(''))
      })

    }
  )
}
