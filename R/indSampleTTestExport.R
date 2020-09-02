#' @import shiny
indSampleTTestExportUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('indSampleTTestExport')

  fchoices <- c("html", "github", "word", "pdf")

  verticalLayout(
    fixedRow(
      column(width = 3, textInput(ns("author"), tl("Author:"), "Geiser C. Challco")),
      column(width = 3, textInput(ns("email"), tl("Email:"), "geiser@usp.br")),
    ),
    checkboxGroupInput(ns("files"), tl("Export formats"), choices = fchoices, selected = "html", width = "100%", inline = T),
    fixedRow(
      column(width = 6, checkboxGroupInput(ns("dvs"), tl("Detailed reports of"), choices = c(""), inline = T, width = "100%")),
      column(width = 2, actionButton(ns("exportTTest"), "Generate Export Files", icon = icon('file-export'))),
      column(width = 2, uiOutput(ns("downloadButtonUI")))
    ),
    fixedRow(
      column(width = 2, shinyTree::shinyTree(ns("streeFiles"))),
      column(width = 10, verbatimTextOutput(ns("renderSelectedFile")))
    )
  )
}


#' @import shiny
indSampleTTestExportMD <- function(id, dataset, dvs = "dvs", iv = "iv") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('indSampleTTestExport')

      riv <- reactiveVal(unique(unlist(dataset$variables[c(iv)], use.names = F)))
      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      observeEvent(dataset$variables, {
        riv(unique(unlist(dataset$variables[c(iv)], use.names = F)))
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
        updateCheckboxGroupInput(session, "dvs", choices = rdvs(), inline = T)
      })

      reportId <- digest::digest(list(
        id = id, dataset = reactiveValuesToList(dataset, all.names = T), dvs = dvs, iv = iv)
        , algo = "xxhash64")
      path <- paste0(getwd(), '/report/ind-ttest/', reportId)
      dir.create(path, showWarnings = F, recursive = T)

      # ... generate reports

      updateFiles <- function(progress) {
        knitr::opts_knit$set(base.dir = NULL)
        inc <- 1/(length(input$files) * (1+length(input$dvs)))

        backup <- reactiveValuesToList(dataset, all.names = T)
        backup[["rds.signature"]] <- paste0('ttest-',as.character(packageVersion("rshinystatistics")))
        backup[["author"]] <- input$author
        backup[["email"]] <- input$email
        saveRDS(backup, file = paste0(path, '/backup.rds'))

        # ... generating markdowns and R scripts

        cat(ind.ttestSummaryAsFile('R', backup, dvs, iv, path = path), file = paste0(path, '/ind.ttest.R'))
        cat(ind.ttestSummaryAsFile('Rmd', backup, dvs, iv), file = paste0(path, '/summary.Rmd'))
        for (dv in rdvs()) write.csv(backup$initTable[[dv]], paste0(path, '/data-',dv,'.csv'))

        for (dv in input$dvs) {
          dir.create(paste0(path,'/',dv), showWarnings = F, recursive = T)
          cat(ind.ttestDetailAsFile('R', backup, dv, iv, path = paste0(path,'/',dv)), file = paste0(path,'/',dv,'/ind.ttest.R'))
          cat(ind.ttestDetailAsFile('Rmd', backup, dv, iv), file = paste0(path,'/',dv,'/ind.ttest.Rmd'))
          write.csv(backup$initTable[[dv]], paste0(path, '/', dv, '/data.csv'))
        }

        # .. generating using rmarkdown
        for (nfile in input$files) {
          progress$inc(inc, detail = paste('Generating', nfile,'file of summary'))
          rmarkdown::render(paste0(path, '/summary.Rmd'), paste0(nfile,'_document'))
        }
        for (dv in input$dvs) {
          for (nfile in input$files) {
            progress$inc(inc, detail = paste('Generating',nfile,'file as detailed reported for ',dv))
            rmarkdown::render(paste0(path,'/',dv,'/ind.ttest.Rmd'), paste0(nfile,'_document'))
          }
        }
      }

      observeEvent(input$exportTTest, {
        validate(
          need(!is.null(dataset$indSampleTTestParams[["hypothesis"]]),
               tl("Please perform the t-test before to generate the files to be exported"))
        )
        if (dataset$isSetup && reportId != dataset$reportId) {
          progress <- shiny::Progress$new()
          progress$set(message = tl("Making files to export independent sample t-test"), value = 0)
          updateFiles(progress)
          on.exit(progress$close())

          dataset$reportId <- reportId

          output$renderSelectedFile <- renderPrint({
            list.files(path, include.dirs = T, recursive = T)
          })
        }
      })

      output$downloadButtonUI <- renderUI({
        if (!is.null(input$exportTTest) && input$exportTTest > 0) {
          downloadButton(ns('downloadZIP'), tl('Download ZIP'))
        }
      })

      output$downloadZIP <- downloadHandler(
        filename = function() { paste0(ns("backup"), reportId, ".zip") },
        content = function(file) { zip::zipr(file, files = paste0(path,"/")) }
      )

    }
  )
}
