#' @import shiny
factorialAnovaExportUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('factorialAnovaExport')

  fchoices <- c("html", "github", "word", "pdf")

  verticalLayout(
    fixedRow(
      column(width = 3, textInput(ns("author"), tl("Author:"), "Geiser C. Challco")),
      column(width = 3, textInput(ns("email"), tl("Email:"), "geiser@usp.br")),
    ),
    checkboxGroupInput(ns("files"), tl("Export formats"), choices = fchoices, selected = "html", width = "100%", inline = T),
    fixedRow(
      column(width = 6, checkboxGroupInput(ns("dvs"), tl("Detailed reports of"), choices = c(""), inline = T, width = "100%")),
      column(width = 2, actionButton(ns("exportAnova"), "Generate Export Files", icon = icon('file-export'))),
      column(width = 2, uiOutput(ns("downloadButtonUI")))
    ),
    fixedRow(
      column(width = 2, shinyTree::shinyTree(ns("streeFiles"))),
      column(width = 10, verbatimTextOutput(ns("renderSelectedFile")))
    )
  )
}


#' @import shiny
factorialAnovaExportMD <- function(id, dataset, dvs = "dvs", between = "between") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('factorialAnovaExport')

      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      observeEvent(dataset$variables, {
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
        updateCheckboxGroupInput(session, "dvs", choices = rdvs(), inline = T)
      })

      reportId <- reactiveVal(
        digest::digest(list(
          id = id, dataset = reactiveValuesToList(dataset, all.names = T),
          dvs = dvs, between = between), algo = "xxhash64")
      )
      path <- reactiveVal(
        paste0(getwd(),'/report/factorial-anova/',reportId())
      )

      # ... generate reports

      updateFiles <- function(progress) {
        dir.create(path(), showWarnings = F, recursive = T)
        knitr::opts_knit$set(base.dir = NULL)
        inc <- 1/(length(input$files) * (1+length(input$dvs)))

        backup <- reactiveValuesToList(dataset, all.names = T)
        backup[["rds.signature"]] <- paste0('factorialAnova-',as.character(packageVersion("rshinystatistics")))
        backup[["author"]] <- input$author
        backup[["email"]] <- input$email
        saveRDS(backup, file = paste0(path(), '/backup.rds'))

        # ... generating markdowns and R scripts

        cat(factorialAnovaSummaryAsFile('R', backup, dvs, between, path = path()), file = paste0(path(), '/factorialAnova.R'))
        cat(factorialAnovaSummaryAsFile('Rmd', backup, dvs, between), file = paste0(path(), '/summary.Rmd'))
        write.csv(backup$initTable , paste0(path(), '/data.csv'))

        for (dv in input$dvs) {
          dir.create(paste0(path(),'/',dv), showWarnings = F, recursive = T)
          cat(factorialAnovaDetailAsFile('R', backup, dv, between, path = paste0(path(),'/',dv)), file = paste0(path(),'/',dv,'/factorialAnova.R'))
          cat(factorialAnovaDetailAsFile('Rmd', backup, dv, between), file = paste0(path(),'/',dv,'/factorialAnova.Rmd'))
          write.csv(backup$initTable, paste0(path(), '/', dv, '/data.csv'))
        }

        # .. generating using rmarkdown
        for (nfile in input$files) {
          progress$inc(inc, detail = paste('Generating', nfile,'file of summary'))
          rmarkdown::render(paste0(path(), '/summary.Rmd'), paste0(nfile,'_document'))
        }
        for (dv in input$dvs) {
          for (nfile in input$files) {
            progress$inc(inc, detail = paste('Generating',nfile,'file as detailed reported for ',dv))
            rmarkdown::render(paste0(path(),'/',dv,'/factorialAnova.Rmd'), paste0(nfile,'_document'))
          }
        }
      }

      observeEvent(input$exportAnova, {
        if (!dataset$isSetup) return(NULL)
        validate(
          need(!is.null(dataset$anovaParams[["hypothesis"]]),
               tl("Please perform the ANOVA test before to generate the files to be exported"))
        )
        progress <- shiny::Progress$new()
        progress$set(message = tl("Making files to export ANOVA"), value = 0)
        updateFiles(progress)
        on.exit(progress$close())

        output$renderSelectedFile <- renderPrint({
          list.files(path(), include.dirs = T, recursive = T)
        })
      })

      output$downloadButtonUI <- renderUI({
        if (!is.null(input$exportAnova) && (input$exportAnova) > 0) {
          downloadButton(ns('downloadZIP'), tl('Download ZIP'))
        }
      })

      output$downloadZIP <- downloadHandler(
        filename = function() { paste0(ns("backup"), reportId(), ".zip") },
        content = function(file) { zip::zipr(file, files = paste0(path(),"/")) }
      )

    }
  )
}
