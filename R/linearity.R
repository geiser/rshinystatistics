
#' @import shiny
linearityUI <- function(id, title = "Linearity test") {
  linearityHTML <- "
<p>Se não houver linearidade aparente entre a covariancia e a variável dependente, é
melhor conduzir teste ANOVA empregando a covariança como variável dependente (between-subject)
</p>
  "

  ns <- NS(id)
  tl <- getTranslator("linearity")

  verticalLayout(
    br(), strong(tl(title)), br(),
    div(HTML(linearityHTML)), br(),
    fixedRow(
      column(width = 3, numericInput(ns("width"), "Width", value = 400, min=100, step = 50)),
      column(width = 3, numericInput(ns("height"), "Height", value = 400, min=100, step = 50)),
      column(width = 3, checkboxInput(ns("showLabel"), tl("Show labels"), value = T))
    ),
    radioButtons(ns('dv'), tl("Dependent variable"), choices = c("dv"), inline = T),
    uiOutput(ns("linearPlotsUI"))
  )
}

#' @import shiny
linearityMD <- function(id, dataset, dvs = "dvs", between = "between", covar = "covar") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('linearity')

      wid <- reactiveVal(dataset$variables$wid)
      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      rbetween <- reactiveVal(unique(unlist(dataset$variables[c(between)], use.names = F)))
      rcovar <- reactiveVal(unique(unlist(dataset$variables[c(covar)], use.names = F)))

      observeEvent(dataset$variables, {
        wid(dataset$variables$wid)
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
        rbetween(unique(unlist(dataset$variables[c(between)], use.names = F)))
        rcovar(unique(unlist(dataset$variables[c(covar)], use.names = F)))
        updateRadioButtons(session, "dv", choices = rdvs(), selected = rdvs()[1], inline = T)
      })

      # ... plots linear plots

      output$linearPlotsUI <- renderUI({
        if (dataset$isSetup && input$dv %in% names(dataset$dataTable)) {
          dat <- dataset$dataTable[[input$dv]]
          gdat <-  dplyr::group_data(dplyr::group_by_at(dat, rbetween()))
          do.call(verticalLayout, lapply(seq(1, nrow(gdat)), FUN = function(i) {
            sgroup <- paste0(unlist(lapply(rbetween(), FUN = function(cname) {
              paste0(cname, ":", as.character(gdat[[cname]][i]))
            })), collapse = ", ")
            verticalLayout(
              p(strong(paste(tl("Assessing linearity in"), input$dv, tl("for"), sgroup))),
              renderPlot({
                params <- list(
                  data = dat[gdat$.rows[[i]],],
                  x = rcovar(), y = input$dv, short.panel.labs = F)
                if (input$showLabel) {
                  params$repel = T
                  params$label = wid()
                }
                do.call(ggpubr::ggscatter, params) + ggplot2::stat_smooth(method = "loess", span = 0.9)
              }, width = input$width, height = input$height)
            )
          }))
        }
      })

    }
  )
}

