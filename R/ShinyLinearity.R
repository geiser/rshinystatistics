#' @import shiny
shinyLinearityUI <- function(id, title = "Linearity test") {
  ns <- NS(id)
  tl <- getTranslator()
  linearityHelp <- paste("Se não houver linearidade aparente entre a covariancia e a variável dependente,",
                         "é melhor conduzir teste ANOVA empregando a covariança como variável dependente (between-subject)")

  verticalLayout(
    h4(tl(title)), helpText(linearityHelp),
    fixedRow(
      column(width = 3, radioButtons(ns('dv'), tl("Dependent variable"), choices = c("dv"), inline = T)),
      column(width = 3, numericInput(ns("width"), "Width", value = 400, min=100, step = 50)),
      column(width = 3, numericInput(ns("height"), "Height", value = 400, min=100, step = 50)),
      column(width = 3, checkboxInput(ns("showLabel"), tl("Show labels"), value = T))
    ),
    fixedRow(
      column(width = 4, radioButtons(ns('method'), tl("Method"), choices = c("lm","loess","glm","gam"), inline = T)),
    ),
    uiOutput(ns("linearPlotsUI"))
  )
}

#' @import shiny
shinyLinearityMD <- function(id, dataset, dvs = "dvs", between = "between", covar = "covar", dataTable = 'dataTable') {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tl <- getTranslator()
    vars <- reactiveValues(
      wid = dataset$variables$wid,
      dvs = unique(unlist(dataset$variables[c(dvs)], use.names = F)),
      between = unique(unlist(dataset$variables[c(between)], use.names = F)),
      covar = unique(unlist(dataset$variables[c(covar)], use.names = F))
    )

    observeEvent(dataset$variables, {
      req(dataset$isSetup)
      vars$wid <- dataset$variables$wid
      vars$dvs <- unique(unlist(dataset$variables[c(dvs)], use.names = F))
      vars$between <- unique(unlist(dataset$variables[c(between)], use.names = F))
      vars$covar <- unique(unlist(dataset$variables[c(covar)], use.names = F))
      updateRadioButtons(session, "dv", choices = vars$dvs, selected = vars$dvs[1], inline = T)
    })

    # ... plots linear plots

    observeEvent(input$method, {
      req(dataset$isSetup)
      dataset$lmethod <- input$method
    })

    output$linearPlotsUI <- renderUI({
      req(dataset$isSetup)
      if (input$dv %in% names(dataset[[dataTable]])) {
        dv <- input$dv
        dat <- dataset[[dataTable]][[dv]]
        gdat <-  dplyr::group_data(dplyr::group_by_at(dat, vars$between))
        do.call(verticalLayout, lapply(seq(1, nrow(gdat)), FUN = function(i) {
          sgroup <- paste0(unlist(lapply(vars$between, FUN = function(cname) {
            paste0(as.character(gdat[[cname]][i]))
          })), collapse = ":")
          verticalLayout(
            p(strong(paste(tl("Assessing linearity in"), sgroup, '(',paste0(vars$between, collapse = ':'),')'))),
            renderPlot({
              params <- list(data = dat[gdat$.rows[[i]],], x = vars$covar, y = dv, short.panel.labs = F)
              if (input$showLabel) {
                params$repel = T
                params$label = vars$wid
              }
              do.call(ggpubr::ggscatter, params) + ggplot2::stat_smooth(method = input$method, span = 0.9)
            }, width = input$width, height = input$height)
          )
        }))
      }
    })

  })
}
