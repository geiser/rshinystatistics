



#' @import shiny
#' @export
wilcoxonApp <- function() {
  shinyApp(ui = fluidPage(nonParamTestUI("wilcoxon")), server = function(input, output, session) {
    observe({ nonParamTestMD("wilcoxon") })
  })
}

#' @import shiny
#' @export
kruskalWallisApp <- function() {
  shinyApp(ui = fluidPage(nonParamTestUI("kruskal")), server = function(input, output, session) {
    observe({ nonParamTestMD("kruskal") })
  })
}

#' @import shiny
#' @export
scheirerRayHareApp <- function() {
  shinyApp(ui = fluidPage(nonParamTestUI("srh")), server = function(input, output, session) {
    observe({ nonParamTestMD("srh") })
  })
}
