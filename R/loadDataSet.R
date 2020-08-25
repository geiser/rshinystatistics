#' @import shiny
loadDataSetUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('loadDataSet')
  verticalLayout(
    uiOutput(ns("paramsUI")),
    uiOutput(ns("setButtonUI"))
  )
}

#' @import shiny
loadDataSetMD <- function(id, var.params = list(), dv.vars = c(), rds.signature = NULL) {

  tl <- getTranslator('loadDataSet')
  var.params <- c(list(wid = list(type = "unique", max = 1, include = c("row.pos"),
                                  label = tl("column of the obs. identifier"))), var.params)

  get_choices <- function(data, type = 'other', values.count = nrow(data), params = list()) {
    if ('type' %in% names(params)) type <- params$type
    if ('values.count' %in% names(params)) values.count <- params$values.count
    unlist(sapply(colnames(data), USE.NAMES = F, FUN = function(cname) {
      if (type == 'numeric') {
        if (is.numeric(data[[cname]])) return(cname)
      } else if (type == 'non.numeric') {
        if (!is.numeric(data[[cname]])) return(cname)
      } else if (type == 'unique') {
        if (length(unique(data[[cname]])) == values.count) return(cname)
      } else {
        return(cname)
      }
    }))
  }

  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      files <- reactiveValues()
      values <- reactiveValues(fileTable = NULL, initTable = NULL, variables = list(), dataTable = NULL, isSetup = F)

      output$paramsUI <- renderUI({
        if (!values$isSetup) {
          verticalLayout(
            fileInput(ns("file"), tl("Choose CSV, Excel or RDS file"), multiple = F, accept= c(".csv", ".xlsx", ".rds")),
            uiOutput(ns("fileOptionsUI")),
            uiOutput(ns("var.paramsUI"))
          )
        }
      })

      output$fileOptionsUI <- renderUI({
        req(input$file)
        encodings <- c("UTF-8", "latin1","WINDOWS-1252","ASCII","BIG5","GB18030","GB2312",
                       "ISO-2022-JP","ISO-2022-KR","ISO-8859-1","ISO-8859-2","ISO-8859-7","SHIFT-JIS")

        ext <- tools::file_ext(input$file$datapath)
        formatFile <- readxl::format_from_signature(input$file$datapath)

        if (!is.na(formatFile) && formatFile == "xlsx") {
          choices = readxl::excel_sheets(input$file$datapath)
          selectInput(ns("excelSheet"), tl("Spreadsheet"), choices = choices, multiple = F)
        } else if (ext == "csv") {
          fileEncoding = readr::guess_encoding(input$file$datapath)$encoding[1]
          verticalLayout(
            radioButtons(ns("csvSep"), tl("Data Separator"), c(Comma = ",", Semicolon = ";", Tab = "\t")),
            radioButtons(ns("csvQuote"), tl("Quoting Characters"), c("Double Quote" = '"', "Single Quote" = "'", None = "")),
            selectInput(ns("csvFileEncoding"), tl("File Encoding"), choices = c("", encodings), selected = fileEncoding, multiple = F),
            selectInput(ns("csvEncoding"), tl("Content Encoding"), choices = c("unknown", encodings), selected = "unknown", multiple = F)
          )
        } else if (ext == "rds") {
          rds <- readRDS(input$file$datapath)
          if (!is.null(rds.signature)) {
            validate(
              need("rshinystatistics.signature" %in% names(rds),
                   tl("Please select a valid RDS file - the file should be created in rshiny-statistics app")),
              need(rds$rds.signature == rds.signature,
                   tl("Signature of rshiny-statistics file doesn't match with the expected value"))
            )
          }
          files$rds <- rds
          checkboxGroupInput(ns("loadDataRDS"), tl("Choose Data to be Loaded"), setdiff(names(rds), "rshinystatistics.signature"))
        }
      })

      # ... dealing with file options

      observeEvent(input$excelSheet, {
        values$fileTable <- readxl::read_excel(input$file$datapath, sheet = input$excelSheet)
      })

      fileTableFromCSV <- function() {
        values$fileTable <- utils::read.csv(
          input$file$datapath,
          sep = input$csvSep, quote = input$csvQuote,
          fileEncoding = input$csvFileEncoding, encoding = input$csvEncoding)
      }

      observeEvent(input$csvSep, { fileTableFromCSV() })
      observeEvent(input$csvQuote, { fileTableFromCSV() })
      observeEvent(input$csvFileEncoding, { fileTableFromCSV() })
      observeEvent(input$csvEncoding, { fileTableFromCSV() })

      observeEvent(input$loadDataRDS, {
        req(files$rds)
        lselected <- as.list(input$loadDataRDS)
        names(lselected) <- input$loadDataRDS
        values <- lapply(lselected, FUN = function(nvar) files$rds[[nvar]])
      })

      # ... dealing with the variables of the data set

      output$var.paramsUI <- renderUI({
        do.call(verticalLayout, lapply(names(var.params), FUN = function(var) {
          lvar <- var.params[[var]]
          label <- ifelse("label" %in% names(lvar), lvar$label, var)
          multiple <- ifelse("max" %in% names(lvar) && lvar$max == 1, F, T)
          if (lvar$type != 'convert.non.numeric') {
            selectInput(ns(var), label, choices = NULL, multiple = multiple)
          } else {
            verticalLayout(
              selectInput(ns(var), label, choices = NULL, multiple = multiple),
              uiOutput(ns(paste0(var,'Options')))
            )
          }
        }))
      })

      lapply(names(var.params), FUN = function(var) {
        lvar <- var.params[[var]]
        if (lvar$type == 'convert.non.numeric') {
          output[[paste0(var,'Options')]] <- renderUI({
            do.call(verticalLayout, lapply(input[[var]], FUN = function(x) {
              if (is.numeric(values$fileTable[[x]])) {
                radioButtons(
                  ns(paste0('quantileFor',var,'In',x)), inline = T,
                  label = paste(tl('Divide'), paste0('"',x,'"'), tl('using quantiles in:')),
                  choiceNames = c("lower/upper","low/medium/high"), choiceValues = c(2,3)
                )
              }
            }))
          })
        }
      })

      get_vars_to_convert_non.numeric <- function() {
        toReturn <- list()
        for (var in names(var.params)) {
          lvar <- var.params[[var]]
          if (lvar$type == 'convert.non.numeric') {
            for (x in input[[var]]) {
              if (is.numeric(values$fileTable[[x]])) {
                toReturn[[x]] <- list(var = x, qq = input[[paste0('quantileFor',var,'In',x)]])
              }
            }
          }
        }
        return(toReturn)
      }

      observeVariableEvents <- list()

      updateVariables <- function() {
        lapply(names(var.params), FUN = function(var) {
          lvar <- var.params[[var]]
          if (!"removeFrom" %in% names(lvar) || length(lvar$removeFrom) == 0) {
            choices <- get_choices(values$fileTable, params = lvar)
            if (length(lvar$include) > 0) choices <- c(choices, lvar$include)
            selected <- choices[1]
            if (!is.null(files$rds) && "variables" %in% names(input$loadDataRDS)
                && var %in% names(files$rds$variables)) {
              selected <- files$rds$variables[[var]]
            }
            updateSelectInput(session, var, choices = choices, selected = selected)
          }
        })

        return(
          lapply(names(var.params), FUN = function(var) {
            lvar <- var.params[[var]]
            if ("removeFrom" %in% names(lvar) && length(lvar$removeFrom) > 0) {
              lapply(lvar$removeFrom, FUN = function(removeVar) {
                observeEvent(input[[removeVar]], {
                  choices <- get_choices(values$fileTable, param = lvar)
                  toRemove <- do.call(c, lapply(lvar$removeFrom, FUN = function(x) input[[x]]))
                  choices <- setdiff(choices, toRemove)

                  selected <- NULL
                  if (!is.null(files$rds) && "variables" %in% names(input$loadDataRDS)
                      && var %in% names(files$rds$variables)) {
                    selected <- files$rds$variables[[var]]
                  }
                  updateSelectInput(session, var, choices = choices, selected = selected)
                })
              })
            }
          })
        )
      }

      observeEvent(values$fileTable, {
        if (!values$isSetup) {
          values$initTable <- NULL
          values$dataTable <- NULL
          values$variables = list()
          observeVariableEvents <- updateVariables()
        }
      })

      lapply(names(var.params), FUN = function(var) {
        observeEvent(input[[var]], {
          if ("max" %in% names(var.params[[var]])) {
            max <- var.params[[var]]$max
            if (max > 1 && length(input[[var]]) > max) {
              updateSelectInput(session, var, selected = input[[var]][1:max])
            }
          }
        })
      })

      lapply(names(var.params), FUN = function(var) {
        observeEvent(input[[var]], {
          values$variables[[var]] <- input[[var]]
        })
      })

      # ... dealing with load data button

      output$setButtonUI <- renderUI({
        if (all(do.call(c, lapply(names(var.params), FUN = function(var) {
          param <- var.params[[var]]
          if ("min" %in% names(param)) length(input[[var]]) >= param[["min"]]
          else length(input[[var]]) > 0
        })))) {
          actionButton(ns("setButton"), tl("Load Data Set"), icon = icon("arrow-circle-up"))
        }
      })

      observeEvent(input$setButton, {
        if (input$setButton %% 2 != 0) {

          cnames <- setdiff(unique(unlist(values$variables, use.names = F)),'row.pos')
          initTable <- values$fileTable[complete.cases(values$fileTable[,cnames]),cnames]
          initTable <- df2qqs(initTable, params = get_vars_to_convert_non.numeric())
          if (input$wid == 'row.pos') {
            initTable <- cbind(row.pos = seq(1, nrow(initTable)), initTable)
          }
          values$initTable <- initTable

          dvs <- unique(unlist(values$variables[dv.vars], use.names = F))
          values$dataTable <- set_datatable(values$initTable, dvs)

          updateActionButton(session, "setButton", tl("Change Data Set"), icon = icon("arrow-circle-down"))
          values$isSetup <- T
          values$reportId <- '000'
        } else {
          values$isSetup <- F
          values$variables <- list()
          nlists <- names(reactiveValuesToList(values))
          for (nlist in nlists[!nlists %in% c('isSetup','variables')]) {
            if (nlist != 'isSetup' || nlist != 'variables') {
              #print(nlist);
              values[[nlist]] <- NULL
            }
          }

          updateActionButton(session, "setButton", tl("Load Data Set"), icon = icon("arrow-circle-up"))
        }
      })

      return(values)
    }
  )
}

