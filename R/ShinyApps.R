#' Shiny Application to Run Wilcoxon Rank Sum Tests
#'
#' This function performs run a shiny application in which you can conduct two-sample Wilcoxon tests
#'
#' @import shiny
#' @export
wilcoxonApp <- function() {
  shinyApp(ui = fluidPage(shinyHypothesisUI("wilcoxon")), server = function(input, output, session) {
    observe({ shinyHypothesisMD("wilcoxon") })
  })
}

#' Shiny Application to Run Kruskal-Wallis Rank Sum Tests
#'
#' This function performs run a shiny application in which you can conduct Kruskal-Wallis rank sum tests
#'
#' @import shiny
#' @export
kruskalApp <- function() {
  shinyApp(ui = fluidPage(shinyHypothesisUI("kruskal")), server = function(input, output, session) {
    observe({ shinyHypothesisMD("kruskal") })
  })
}

#' Shiny Application to Run Scheirer Ray Hare Tests
#'
#' This function performs run a shiny application in which you can conduct Scheirer-Ray hare tests
#'
#' @import shiny
#' @export
scheirerApp <- function() {
  shinyApp(ui = fluidPage(shinyHypothesisUI("srh")), server = function(input, output, session) {
    observe({ shinyHypothesisMD("srh") })
  })
}

#' Shiny Application to Run Factorial ANCOVA Tests
#'
#' This function performs run a shiny application in which you can conduct Factorial ANCOVA tests
#'
#' @import shiny
#' @export
ancovaApp <- function() {
  shinyApp(ui = fluidPage(shinyHypothesisUI("ancova")), server = function(input, output, session) {
    observe({ shinyHypothesisMD("ancova") })
  })
}

#' Shiny Application to Run Factorial ANOVA Tests
#'
#' This function performs run a shiny application in which you can conduct Factorial ANOVA tests
#'
#' @import shiny
#' @export
anovaApp <- function() {
  shinyApp(ui = fluidPage(shinyHypothesisUI("anova")), server = function(input, output, session) {
    observe({ shinyHypothesisMD("anova") })
  })
}

#' Shiny Application to Run Student's T-Tests
#'
#' This function performs run a shiny application in which you can conduct Student's t-tests
#'
#' @import shiny
#' @export
ttestApp <- function() {
  shinyApp(ui = fluidPage(shinyHypothesisUI("ttest")), server = function(input, output, session) {
    observe({ shinyHypothesisMD("ttest") })
  })
}
