
#' @import shiny
#' @export
wilcoxonApp <- function() {
  shinyApp(ui = fluidPage(compHypothesisUI("wilcoxon")), server = function(input, output, session) {
    observe({ compHypothesisMD("wilcoxon") })
  })
}

#' @import shiny
#' @export
kruskalWallisApp <- function() {
  shinyApp(ui = fluidPage(compHypothesisUI("kruskal")), server = function(input, output, session) {
    observe({ compHypothesiMD("kruskal") })
  })
}

#' @import shiny
#' @export
scheirerRayHareApp <- function() {
  shinyApp(ui = fluidPage(compHypothesisUI("srh")), server = function(input, output, session) {
    observe({ compHypothesisMD("srh") })
  })
}

#' @import shiny
#' @export
ancovaApp <- function() {
  shinyApp(ui = fluidPage(compHypothesisUI("ancova")), server = function(input, output, session) {
    observe({ compHypothesisMD("ancova") })
  })
}

