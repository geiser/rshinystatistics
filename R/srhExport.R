#' @import shiny
srhExportUI <- function(id) {
  ns <- NS(id)
  tl <- getTranslator('srhExport')

  fchoices <- c("html", "github", "word")

  verticalLayout(
    fixedRow(
      column(width = 3, textInput(ns("author"), tl("Author:"), "Geiser C. Challco")),
      column(width = 3, textInput(ns("email"), tl("Email:"), "geiser@alumni.usp.br")),
    ),
    checkboxGroupInput(ns("files"), tl("Export formats"), choices = fchoices, selected = "html", width = "100%", inline = T),
    fixedRow(
      column(width = 2, uiOutput(ns("exportButtonUI"))),
      column(width = 2, uiOutput(ns("space1UI"))),
      column(width = 2, uiOutput(ns("downloadButtonUI")))
    ),
    fixedRow(
      column(width = 2, shinyTree::shinyTree(ns("streeFiles"))),
      column(width = 10, verbatimTextOutput(ns("renderSelectedFile")))
    )
  )
}


#' @import shiny
srhExportMD <- function(id, dataset, dvs = "dvs", between = "between", initTable='initTable', dataTable = 'dataTable', fileTable='fileTable') {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      tl <- getTranslator('srhExport')

      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      observeEvent(dataset$variables, {
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
        updateCheckboxGroupInput(session, "dvs", choices = rdvs(), inline = T)
      })

      reportId <- reactiveVal(
        digest::digest(list(
          id = id, dataTable = dataset[[dataTable]], all.names = T,
          srhParams = dataset$srhParams, dvs = dvs, between = between), algo = "xxhash64")
      )

      path <- reactiveVal(paste0(getwd(),'/report/srh/',reportId()))

      # ... generate reports

      updateFiles <- function(progress) {
        dir.create(path(), showWarnings = F, recursive = T)
        dir.create(paste0(path(),'/code'), showWarnings = F, recursive = T)
        dir.create(paste0(path(),'/data'), showWarnings = F, recursive = T)
        dir.create(paste0(path(),'/results'), showWarnings = F, recursive = T)

        knitr::opts_knit$set(base.dir = NULL)
        inc <- 1/(length(input$files) * (1+length(input$dvs)))

        backup <- reactiveValuesToList(dataset, all.names = T)
        backup[["rds.signature"]] <- paste0('srh-', as.character(packageVersion("rshinystatistics")))
        backup[["author"]] <- input$author
        backup[["email"]] <- input$email

        # ... saving data
        saveRDS(backup, file = paste0(path(), '/data/backup.rds'))
        write.csv(backup[[fileTable]], paste0(path(),'/data/initial-table.csv'))
        for (dv in rdvs()) write.csv(backup[[initTable]][[dv]], paste0(path(),'/data/table-for-',dv,'.csv'))

        # ... saving code
        cat(srhAsFile('Rmd', backup, dvs, between), file = paste0(path(),'/code/srh.Rmd'))
        knitr::purl(paste0(path(),'/code/srh.Rmd'), paste0(path(),'/code/srh.R'), documentation = 2)

        # ... generating reports
        for (nfile in input$files) {
          progress$inc(inc, detail = paste('Generating', nfile,'files for the report'))
          rmarkdown::render(paste0(path(), '/code/srh.Rmd'), paste0(nfile,'_document'), output_dir = paste0(path(),'/results'))
        }
      }

      observeEvent(input[[paste0("exportSRH",reportId())]], {
        if (!dataset$isSetup) return(NULL)
        validate(
          need(!is.null(dataset$srhParams[["hypothesis"]]),
               tl("Please perform the SRH test before to generate the files to be exported"))
        )
        progress <- shiny::Progress$new()
        progress$set(message = tl("Making files to export SRH test"), value = 0)

        updateFiles(progress)
        on.exit(progress$close())

        output$renderSelectedFile <- renderPrint({
          list.files(path(), include.dirs = T, recursive = T)
        })
      })

      output$exportButtonUI <- renderUI({
        if (!dataset$isSetup) return(NULL)
        actionButton(ns(paste0("exportSRH",reportId())), tl("Generate Export Files"), icon = icon('file-export'))
      })

      output$downloadButtonUI <- renderUI({
        if (!is.null(input[[paste0("exportSRH", reportId())]]) && (input[[paste0("exportSRH", reportId())]]) > 0) {
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
