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
loadDataSetMD <- function(id, var.params = list(), dv.vars = c(), rds.signature = NULL, include.diffTable = F) {

  tl <- getTranslator('loadDataSet')
  var.params <- c(list(wid = list(type = "non.numeric", min = 1, max = 1, include = c("row.pos"),
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
          if (lvar$type == 'repeated.measures') {
            num_max <- 20
            if ('max' %in% names(lvar)) num_max <- lvar$max
            verticalLayout(
              numericInput(ns(paste0('num',var)), tl('Number of measurements'), value=lvar$min, min=lvar$min, max=num_max),
              uiOutput(ns(paste0(var,'Options')))
            )
          } else if (lvar$type != 'convert.non.numeric') {
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
        if (lvar$type == 'repeated.measures') {
          output[[paste0(var,'Options')]] <- renderUI({
            colchoices <-   get_choices(values$fileTable, 'numeric')
            do.call(verticalLayout, lapply(seq(1,input[[paste0('num',var)]]), FUN = function(i) {
              verticalLayout(
                textInput(ns(paste0(var,i,'Value')), paste0(tl('Name for the dependent variable'),' ',i), value = paste0('score',i)),
                textInput(ns(paste0(var,i,'Key')), tl('Key name for the dependent variable'), value = paste0('time',i)),
                selectInput(ns(paste0(var,i)),stringr::str_replace_all(lvar$label,'%i',as.character(i)), choices = colchoices, multiple = T)
              )
            }))
          })
        } else if (lvar$type == 'convert.non.numeric') {
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

          if ("max.depend.on" %in% names(lvar) && "max" %in% names(lvar) &&
              lvar$max - length(input[[lvar[['max.depend.on']]]]) < 1) {
            updateSelectInput(session, var, choices = c(''), selected = c(''))
          } else if (!"removeFrom" %in% names(lvar) || length(lvar$removeFrom) == 0) {
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
                  if ("max.depend.on" %in% names(lvar) && "max" %in% names(lvar) &&
                      lvar$max - length(input[[lvar[['max.depend.on']]]]) < 1) {
                    updateSelectInput(session, var, choices = c(''), selected = c(''))
                  } else if (!is.null(files$rds) && "variables" %in% names(input$loadDataRDS)
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
        if (var.params[[var]]$type != 'repeated.measures') {
          observeEvent(input[[var]], {
            if ("max" %in% names(var.params[[var]])) {
              max <- var.params[[var]]$max
              if ("max.depend.on" %in% names(var.params[[var]])) {
                max <- max - length(input[[var.params[[var]][['max.depend.on']]]])
                if (max > 0)
                  updateSelectInput(session, var, selected = input[[var]][1:max])
                else {
                  updateSelectInput(session, var, choices = c(''), selected = c(''))
                }
              } else if (max > 1 && length(input[[var]]) > max) {
                updateSelectInput(session, var, selected = input[[var]][1:max])
              }
            }
          })
        }
      })

      observe({
        lapply(names(var.params), FUN = function(var) {
          lvar <- var.params[[var]]
          if (lvar$type == 'repeated.measures') {
            i <- input[[paste0('num',var)]]
            if (!is.null(i) && i > 0) {
              vals <- do.call(c, lapply(seq(1,input[[paste0('num',var)]]), FUN = function(i) {
                if ("max.measures" %in% names(lvar)) {
                  max <- lvar$max.measures
                  if (length(input[[paste0(var,i)]]) > max) {
                    updateSelectInput(session, paste0(var,i), selected = input[[paste0(var,i)]][1:max])
                  }
                }
              }))
            }
          }
        })
      })

      observe({
        lapply(names(var.params), FUN = function(var) {
          lvar <- var.params[[var]]
          if (lvar$type == 'repeated.measures') {
            i <- input[[paste0('num',var)]]
            if (!is.null(i) && i > 0) {
              vals <- do.call(c, lapply(seq(1,input[[paste0('num',var)]]), FUN = function(i) {
                input[[paste0(var,i,'Value')]]
              }))
              values$variables[[var]] <- vals

              lvars <- as.list(vals); names(lvars) <- vals
              keys <- lapply(lvars, FUN = function(v) {
                input[[paste0(var,which(vals == v),'Key')]]
              })
              values$variables[[paste0(var,'.within')]] <- keys
              measurement <- lapply(lvars, FUN = function(v) {
                input[[paste0(var,which(vals == v))]]
              })
              values$variables[[paste0(var,'.measurements')]] <- measurement
            }
          } else if ("max.depend.on" %in% names(lvar) && "max" %in% names(lvar) &&
                     lvar$max - length(input[[lvar[['max.depend.on']]]]) < 1) {
            values$variables[[var]] <- c()
          } else if (var.params[[var]]$type != 'repeated.measures') {
            values$variables[[var]] <- input[[var]]
          }
        })
      })


      # ... dealing with load data button

      output$setButtonUI <- renderUI({
        if (all(do.call(c, lapply(names(var.params), FUN = function(var) {
          param <- var.params[[var]]
          if (param$type == "repeated.measures") {
            bvals <- do.call(c, lapply(seq(1,input[[paste0('num',var)]]), FUN = function(i) {
              bmin <- 2
              if ('min.measures' %in% names(param))
                bmin <- param$min.measures

              length(input[[paste0(var,i)]]) >= bmin &&
                stringr::str_count(input[[paste0(var,i,'Value')]]) > 0 &&
                stringr::str_count(input[[paste0(var,i,'Key')]]) > 0
            }))
          } else if ("min" %in% names(param)) {
            bvals <- length(input[[var]]) >= param[["min"]]
          } else{
            bvals <- length(input[[var]]) > 0
          }
          return(all(bvals))
        })))) {
          actionButton(ns("setButton"), tl("Load Data Set"), icon = icon("arrow-circle-up"))
        }
      })

      observeEvent(input$setButton, {
        if (input$setButton %% 2 != 0) {

          cnames <- setdiff(unique(unlist(values$variables, use.names = F)),'row.pos')
          cnames <- cnames[cnames %in% colnames(values$fileTable)]
          initTable <- values$fileTable[complete.cases(values$fileTable[,cnames]),cnames]
          initTable <- df2qqs(initTable, params = get_vars_to_convert_non.numeric())
          if (input$wid == 'row.pos') {
            initTable <- cbind(row.pos = seq(1, nrow(initTable)), initTable)
          }

          ldvs <- values$variables[[dv.vars]]
          names(ldvs) <- values$variables[[dv.vars]]
          if (var.params[[dv.vars]][['type']] == 'repeated.measures') {
            initTable2 <- lapply(ldvs, FUN = function(dv) {
              cols <- values$variables[[paste0(dv.vars,'.measurements')]][[dv]]
              cnames <- unlist(values$variables[!names(values$variables) %in%
                                                  c(dv.vars, paste0(dv.vars, c('.measurements','.within')))]
                               , use.names = F)
              keys <- values$variables[[paste0(dv.vars,'.within')]][[dv]]
              df <- tidyr::pivot_longer(initTable, cols = cols, names_to = keys, values_to = dv)
              return(df[,c(cnames,keys,dv)])
            })
          } else {
            initTable2 <- lapply(ldvs, FUN = function(dv) {
              cnames <-  unlist(values$variables[!names(values$variables) %in% dv.vars], use.names = F)
              initTable[,c(cnames,dv)]
            })
          }


          values$initTable <- initTable2

          dvs <- unique(unlist(values$variables[dv.vars], use.names = F))
          values$dataTable <- initTable2

          # diff table
          if (include.diffTable &&  var.params[[dv.vars]][['type']] == "repeated.measures") {
            measurements <- values$variables[[paste0(dv.vars,'.measurements')]]

            values$diffTable <- lapply(names(measurements), FUN = function(dv) {
              dat <- values$fileTable
              if (input$wid == 'row.pos') {
                dat <- cbind(row.pos = seq(1, nrow(dat)), dat)
              }

              vals <- measurements[[dv]]
              dat[[dv]] <- dat[[vals[length(vals)]]]
              for (i in seq(length(vals)-1, 1)) {
                dat[[dv]] <- dat[[dv]] - dat[[vals[i]]]
              }
              return(dat[,c(input$wid, dv)])
            })
            names(values$diffTable) <- names(measurements)
            values$variables[[paste0(dv.vars,'.diff')]] <- names(values$diffTable)
          }

          updateActionButton(session, "setButton", tl("Change Data Set"), icon = icon("arrow-circle-down"))
          values$isSetup <- T
          values$reportId <- '000'
        } else {
          values$isSetup <- F
          values$variables <- list()
          nlists <- names(reactiveValuesToList(values))
          for (nlist in nlists[!nlists %in% c('isSetup','variables')]) {
            if (nlist != 'isSetup' || nlist != 'variables') {
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

