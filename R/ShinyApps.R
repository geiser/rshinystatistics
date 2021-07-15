
#' @import shiny
#' @export
carelessApp <- function() {
  shinyApp(ui = fluidPage(shinyCarelessUI("carelessApp")), server = function(input, output) {
    observe({ shinyCarelessMD("carelessApp") })
  })
}

#' @import shiny
#' @export
wilcoxonApp <- function() {
  shinyApp(ui = fluidPage(shinyHypothesisUI("wilcoxon")), server = function(input, output, session) {
    observe({ shinyHypothesisMD("wilcoxon") })
  })
}

#' @import shiny
#' @export
kruskalApp <- function() {
  shinyApp(ui = fluidPage(shinyHypothesisUI("kruskal")), server = function(input, output, session) {
    observe({ shinyHypothesisMD("kruskal") })
  })
}

#' @import shiny
#' @export
scheirerApp <- function() {
  shinyApp(ui = fluidPage(shinyHypothesisUI("srh")), server = function(input, output, session) {
    observe({ shinyHypothesisMD("srh") })
  })
}

#' @import shiny
#' @export
ancovaApp <- function() {
  shinyApp(ui = fluidPage(shinyHypothesisUI("ancova")), server = function(input, output, session) {
    observe({ shinyHypothesisMD("ancova") })
  })
}

#' @import shiny
#' @export
anovaApp <- function() {
  shinyApp(ui = fluidPage(shinyHypothesisUI("anova")), server = function(input, output, session) {
    observe({ shinyHypothesisMD("anova") })
  })
}

#' @import shiny
#' @export
ttestApp <- function() {
  shinyApp(ui = fluidPage(shinyHypothesisUI("ttest")), server = function(input, output, session) {
    observe({ shinyHypothesisMD("ttest") })
  })
}
