#' @import shiny
shinyDisplayCarelessUI <- function(id) {

  ns <- NS(id)
  tl <- getTranslator()

  coutliers <- as.list(c("outliers", "suspectedoutliers", "all"))
  names(coutliers) <- c(tl("Only outliers"), tl("Possible outliers"), tl("All points"))

  verticalLayout(
    br(),
    h4(tl("Careless responses by longstring")),
    radioButtons(ns("boxpoints"), tl("Point Display"), inline = T, choices = coutliers, selected = "outliers"),
    splitLayout(
      plotly::plotlyOutput(ns("bpWithCareless")),
      plotly::plotlyOutput(ns("bpWithoutCareless"))
    ),
    br(),
    h4(tl("Careless responses by Intra-individual Response Variability (IRV)")),
    sliderInput(ns("irvLimit"), tl("IRV limit"), min = 0, max = 5, value = 0.5, step = 0.1),
    DT::dataTableOutput(ns("irvTable")),
    br(),
    h4(tl("Initial table without careless responses")),
    DT::dataTableOutput(ns("initTableWithoutCareless"))
  )
}

#' @import shiny
shinyDisplayCarelessMD <- function(id, dataset) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      tl <- getTranslator()

      output$bpWithCareless <- plotly::renderPlotly({
        if (dataset$isSetup) {
          wid <- dataset$variables$wid
          items <- dataset$variables$items
          dat <- dataset$initTable[,c(wid,items)]
          dat[["resp"]] <- rep('resp', nrow(dat))
          dat[["longstring"]] <- careless::longstring(dat[,items])
          boxPlotly(dat, "longstring", "resp", wid, input$boxpoints, title = "Longstring With Careless")
        }
      })

      output$bpWithoutCareless <- plotly::renderPlotly({
        if (dataset$isSetup) {
          wid <- dataset$variables$wid
          items <- dataset$variables$items
          dat <- dataset$dataTable[,c(wid,items)]
          dat[["resp"]] <- rep('resp', nrow(dat))
          dat[["longstring"]] <- careless::longstring(dat[,items])
          boxPlotly(dat, "longstring", "resp", wid, input$boxpoints, title = "Longstring Without Careless")
        }
      })

      observeEvent(dataset$isSetup, {
        if (dataset$isSetup) {
          items <- dataset$variables$items
          updateSliderInput(session, 'irvLimit', min = 0, max = max(dataset$initTable[,items]), value = 0.5, step = 0.1)
        }
      })

      output$irvTable <- DT::renderDataTable({
        if (dataset$isSetup) {
          wid <- dataset$variables$wid
          items <- dataset$variables$items
          dat <- dataset$initTable[,c(wid,items)]
          dat[["irv"]] <- careless::irv(dat[,items])
          dat <- subset(dat, irv <= input$irvLimit)
          df2DT(dat, prefix = ns("irv-tbl"))
        }
      })

      output$initTableWithoutCareless <- DT::renderDataTable({
        if (dataset$isSetup) {
          wid <- dataset$variables$wid
          dat <- dataset$initTable[!dataset$initTable[[wid]] %in% dataset$careless,]
          df2DT(dat, prefix = ns("tbl-without-careless"))
        }
      })

    }
  )
}
