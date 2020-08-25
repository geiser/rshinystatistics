
df2DT <- function(df, pageLength = -1, editable = FALSE, footbuttons = NULL, filename = NULL, prefix = 'table') {
  if (length(dim(df)) > 0) {

    if (is.null(filename)) {
      filename = paste0(prefix, '-', digest::digest(df, algo = "crc32"))
    }

    clengths <- c(25, 50, 100, -1)
    if (!pageLength  %in% clengths) clengths <- c(pageLength, clengths)

    params = list(
      data = df, escape = F, rownames = F, extensions = c("Buttons"),
      class = 'cell-border compact stripe', editable = editable,
      options = list(
        pageLength = pageLength, dom = 'Bfrtip', filter = 'top',
        buttons = list('pageLength','copy','print',
                       list(
                         extend = 'collection',
                         buttons = list(
                           list(extend = 'csv', filename = filename),
                           list(extend = 'excel', filename = filename),
                           list(extend = 'pdf', filename = filename)),
                         text = 'Download')),
        lengthMenu = list(clengths,  paste(clengths, 'rows')),
        columnDefs = list(list(targets = 0:(length(names(df))-1))))
    )

    if (nrow(df) > 100) params$filter = "top"

    if (!is.null(footbuttons) && length(footbuttons) > 0) {
      params$container = tags$table(tableHeader(colnames(df)), tableFooter(c(footbuttons)))
    }

    do.call(DT::datatable, params)
  }
}

#' @import shiny
df2TableUI <- function(id, digits = -1) {
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
df2TableMD <- function(id, df, columns = NULL, pageLength = -1, filename = NULL, prefix = 'table') {
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
