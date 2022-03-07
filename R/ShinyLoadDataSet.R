#' @import shiny
shinyLoadDataSetUI <- function(id) {
  shinyjs::useShinyjs()
  ns <- NS(id)
  verticalLayout(uiOutput(ns("paramsUI")), uiOutput(ns("setButtonUI")))
}

#' @import shiny
shinyLoadDataSetMD <- function(id, var.params = list(), dv.vars = NULL, rds.signature = NULL, include.diffTable = F) {

  tl <- getTranslator()

  var.params <- c(list(wid = list(type = "unique", min = 1, max = 1, include = c("row.pos"),
                                  label = tl("column of the obs. identifier"))), var.params)

  get_choices <- function(data, type = 'other', values.count = nrow(data), params = list()) {
    if ('type' %in% names(params)) {
      type <- params$type
    }
    if ('values.count' %in% names(params)) {
      values.count <- params$values.count
    }
    unlist(sapply(colnames(data), USE.NAMES = F, FUN = function(cname) {
      if (type == 'numeric') {
        if (is.numeric(data[[cname]])) return(cname)
      } else if (type == 'as.categorical') {
        if (!is.numeric(data[[cname]])) return(cname)
      } else if (type == 'unique') {
        if (length(unique(data[[cname]])) == values.count) return(cname)
      } else {
        return(cname)
      }
    }))
  }

  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    files <- reactiveValues()
    values <- reactiveValues(fileTable = NULL, initTable = NULL, variables = list(), dataTable = NULL, isSetup = F)

    output$paramsUI <- renderUI({
      if (!values$isSetup)
        verticalLayout(
          fileInput(ns("file"), tl("Choose csv, excel or backup.rds file"), multiple = F, accept= c(".csv", ".xlsx", ".rds")),
          uiOutput(ns("fileOptionsUI")), uiOutput(ns("var.paramsUI"))
        )
    })

    # UI to load data from file
    ################################
    output$fileOptionsUI <- renderUI({

      req(input$file)
      ext <- tools::file_ext(input$file$datapath)

      if (ext == "rds") {
        rds <- readRDS(input$file$datapath)
        if (!is.null(rds.signature)) {
          rds.version <- as.numeric_version(gsub("[^0-9.]", '', rds$rds.signature))
          validate(
            need("rshinystatistics.signature" %in% names(rds),
                 tl("Please select a valid RDS file - the file should be created using rshinystatistics package")),
            need(rds.version > as.numeric_version(gsub("[^0-9.]", '', rds.signature)),
                 tl("Backup RDS file is higher that your current version - update your rshinystatistics package"))
          )
        }
      } else if (ext == "csv") {
        encodings <- c("UTF-8", "latin1","WINDOWS-1252","ASCII","BIG5","GB18030","GB2312",
                       "ISO-2022-JP","ISO-2022-KR","ISO-8859-1","ISO-8859-2","ISO-8859-7","SHIFT-JIS")
        fileEncoding <- readr::guess_encoding(input$file$datapath)$encoding[1]
        verticalLayout(
          radioButtons(ns("csvSep"), tl("Data Separator"), c(Comma = ",", Semicolon = ";", Tab = "\t")),
          radioButtons(ns("csvQuote"), tl("Quoting Characters"), c("Double Quote" = '"', "Single Quote" = "'", None = "")),
          selectInput(ns("csvFileEncoding"), tl("File Encoding"), choices = c("", encodings), selected = fileEncoding, multiple = F),
          selectInput(ns("csvEncoding"), tl("Content Encoding"), choices = c("unknown", encodings), selected = "unknown", multiple = F)
        )
      } else {
        formatFile <- readxl::format_from_signature(input$file$datapath)
        if (!is.na(formatFile) && formatFile == "xlsx") {
          selectInput(ns("excelSheet"), tl("Spreadsheet"), choices = readxl::excel_sheets(input$file$datapath), multiple = F)
        }
      }
    })

    # load file table
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

    observeEvent(input$excelSheet, {
      values$fileTable <- readxl::read_excel(input$file$datapath, sheet = input$excelSheet)
    })

    # UI to setting up the variables
    ################################
    output$var.paramsUI <- renderUI({
      do.call(verticalLayout, lapply(names(var.params), FUN = function(var) {
        lvar <- var.params[[var]]
        choices <- get_choices(values$fileTable, param = lvar)
        label <- ifelse("label" %in% names(lvar), lvar$label, var)
        multiple <- ifelse("max" %in% names(lvar) && lvar$max == 1, F, T)
        if (lvar$type != 'as.categorical')
          selectInput(ns(var), label, choices = choices, multiple = multiple)
        else
          verticalLayout(
            selectInput(ns(var), label, choices = choices, multiple = multiple),
            uiOutput(ns(paste0(var,'Options'))))
      }))
    })

    # UI to convert numeric column into categorical values
    lapply(names(var.params), FUN = function(var) {
      lvar <- var.params[[var]]
      if (lvar$type == 'as.categorical') {
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

    # ensuring that the max number of variable is adequate
    lapply(names(var.params), FUN = function(var) {
      observeEvent(input[[var]], {
        max <- 0
        if ("max" %in% names(var.params[[var]])) {
          max <- var.params[[var]]$max
          if ("max.depend.on" %in% names(var.params[[var]]))
            max <- max - length(input[[var.params[[var]][['max.depend.on']]]])
        }
        if (max > 0 && length(input[[var]]) > max)
          updateSelectInput(session, var, selected = input[[var]][1:max])
      })
    })

    # update parameters for variables
    observe({
      if (values$isSetup) {
        lapply(names(var.params), FUN = function(var) {
          lvar <- var.params[[var]]
          if ("max.depend.on" %in% names(lvar) && "max" %in% names(lvar) &&
              lvar$max - length(input[[lvar[['max.depend.on']]]]) < 1)
            values$variables[[var]] <- c()
          else
            values$variables[[var]] <- input[[var]]
        })
      }
    })

    #
    lapply(names(var.params), FUN = function(var) {
      lvar <- var.params[[var]]
      if ("removeFrom" %in% names(lvar) && length(lvar$removeFrom) > 0) {
        lapply(lvar$removeFrom, FUN = function(removeVar) {
          observeEvent(input[[removeVar]], {
            choices <- get_choices(values$fileTable, param = lvar)
            choices <- setdiff(choices, do.call(c, lapply(lvar$removeFrom, FUN = function(x) input[[x]])))
            updateSelectInput(session, var, choices = choices, selected = NULL)
          })
        })
      }
    })

    # UI of setting button
    ######################
    output$setButtonUI <- renderUI({
      if (all(do.call(c, lapply(names(var.params), FUN = function(var) {
        param <- var.params[[var]]
        if ("min" %in% names(param))
          return(length(input[[var]]) >= param[["min"]])
        else
          return(length(input[[var]]) > 0)
      })))) {
        actionButton(ns("setButton"), tl("Load Data Set"), icon = icon("arrow-circle-up"))
      }
    })

    observeEvent(input$setButton, {
      if (input$setButton %% 2 != 0) {
        updateActionButton(session, "setButton", tl("Change Data Set"), icon = icon("arrow-circle-down"))
        values$reportId <- '000'
        values$isSetup <- TRUE
      } else {
        updateActionButton(session, "setButton", tl("Load Data Set"), icon = icon("arrow-circle-up"))
        values$variables = list()
        values$initTable <- NULL
        values$dataTable <- NULL
        values$isSetup <- FALSE
      }
    })

    # load data table
    observeEvent(values$isSetup, {
      if (values$isSetup) {
        cnames <- setdiff(unique(unlist(values$variables, use.names = F)),'row.pos')
        cnames <- cnames[cnames %in% colnames(values$fileTable)]
        initTable <- values$fileTable[complete.cases(values$fileTable[,cnames]),]

        qqparams <- list()
        for (var in names(var.params)) {
          if (var.params[[var]][['type']] == 'as.categorical') {
            for (x in input[[var]]) {
              qqparams[[x]] <- list(is.numeric = F, var = x)
              if (is.numeric(values$fileTable[[x]]))
                qqparams[[x]] <- list(is.numeric = T, var = x, qq = input[[paste0('quantileFor',var,'In',x)]])
            }
          }
        }
        if (length(qqparams) > 0) initTable <- df2qqs(initTable, params = qqparams)

        if (input$wid == 'row.pos')
          initTable <- cbind(row.pos = seq(1, nrow(initTable)), initTable)

        # ... generate tables
        values$initTable <- initTable
        values$dataTable <- initTable

        if (!is.null(dv.vars)) {
          ldvs <- values$variables[[dv.vars]]
          names(ldvs) <- values$variables[[dv.vars]]
          values$initTable <- lapply(ldvs, FUN = function(dv) { return(initTable) })
          values$dataTable <- values$initTable
        }
      }
    })

    return(values)
  })
}

