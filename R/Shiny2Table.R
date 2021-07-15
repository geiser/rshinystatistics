
#' @import shiny
shiny2TableUI <- function(id, digits = -1) {
  ns <- NS(id)
  verticalLayout(fixedRow(
    column(width = 12, fixedRow(
      column(width = 10, uiOutput(ns('colnamesInput'))),
      column(width = 2, numericInput(ns("digits"), 'Decimais:', value = digits, step = 1))
    ))),
    DT::DTOutput(ns("dataframeDT"))
  )
}

#' @import shiny
shiny2TableMD <- function(id, df, columns = NULL, pageLength = -1, filename = NULL, prefix = 'table') {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$colnamesInput <- renderUI({
        choices <- colnames(df)
        if (is.null(columns)) columns <- choices else {
          choices <- c(columns,  setdiff(choices, columns))
        }
        selectInput(ns('colnames'), 'Colunas', choices = choices, selected = columns, multiple = T, width = '100%')
      })

      output$dataframeDT <-  DT::renderDataTable({
        if (!is.null(df) && nrow(df) > 0 ) {
          if (input$digits >= 0) {
            for (cname in colnames(df)[sapply(colnames(df), function(x) is.numeric(df[[x]]))]) {
              df[[cname]] <- round(df[[cname]], digits = input$digits)
            }
          }
          cnames <- input$colnames[input$colnames %in% colnames(df)]
          df2DT(df[, cnames], pageLength = pageLength, filename = filename, prefix = prefix)
        }
      })
    }
  )
}
