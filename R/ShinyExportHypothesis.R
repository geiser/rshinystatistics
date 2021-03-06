#' @import shiny
shinyExportHypothesisUI <- function(id) {

  ns <- NS(id)
  tl <- getTranslator()

  fchoices <- c("html", "github", "word")

  verticalLayout(
    fixedRow(
      column(width = 3, textInput(ns("author"), tl("Author:"), "Geiser C. Challco")),
      column(width = 3, textInput(ns("email"), tl("Email:"), "geiser@alumni.usp.br")),
    ),
    helpText(paste(tl("The author and email values are only used to generate the report."),
                   tl("You won't receive a email of the report. Push the button"),'"',
                   tl('Generate Export Files'),'" ', tl('and the button'),'"',tl('Download ZIP'),'"',
                   tl("to obtain the report as ZIP file"))),
    checkboxGroupInput(ns("files"), tl("Report formats"), choices = fchoices, selected = "html", width = "100%", inline = T),
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
shinyExportHypothesisMD <- function(id, test, dataset, dvs = "dvs", between = "between", covar = "covar", initTable='initTable', dataTable = 'dataTable', fileTable='fileTable') {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      tl <- getTranslator()

      rdvs <- reactiveVal(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
      rbetween <- reactiveVal(unique(unlist(dataset$variables[c(between)], use.names = F)))
      rcovar <- reactiveVal(unique(unlist(dataset$variables[c(covar)], use.names = F)))

      observeEvent(dataset$variables, {
        if (!dataset$isSetup) return(NULL)
        rdvs(unique(unlist(dataset$variables[c(dvs)], use.names = F)))
        rbetween(unique(unlist(dataset$variables[c(between)], use.names = F)))
        rcovar(unique(unlist(dataset$variables[c(covar)], use.names = F)))
      })

      reportId <- reactiveVal(
        digest::digest(list(
          id = id, test = test,
          fileTable = dataset[[fileTable]], dataTable = dataset[[dataTable]],
          dvs = dvs, between = between, covar = covar), algo = "xxhash64")
      )

      path <- reactiveVal(paste0(getwd(),'/report/',test,'/',reportId()))

      # ... generate reports

      updateFiles <- function(progress) {
        dir.create(path(), showWarnings = F, recursive = T)
        dir.create(paste0(path(),'/code'), showWarnings = F, recursive = T)
        dir.create(paste0(path(),'/data'), showWarnings = F, recursive = T)
        dir.create(paste0(path(),'/environment'), showWarnings = F, recursive = T)
        dir.create(paste0(path(),'/metadata'), showWarnings = F, recursive = T)
        dir.create(paste0(path(),'/results'), showWarnings = F, recursive = T)

        knitr::opts_knit$set(base.dir = NULL)
        inc <- 1/length(input$files)

        backup <- reactiveValuesToList(dataset, all.names = T)
        backup[["rds.signature"]] <- paste0(test, '-', as.character(packageVersion("rshinystatistics")))
        backup[["author"]] <- input$author
        backup[["email"]] <- input$email

        # ... saving data
        saveRDS(backup, file = paste0(path(), '/backup.rds'))
        write.csv(backup[[fileTable]], paste0(path(),'/data/initial-table.csv'))
        for (dv in rdvs()) write.csv(backup[[initTable]][[dv]], paste0(path(),'/data/table-for-',dv,'.csv'))
        file.copy(system.file("templates/data", "LICENSE", package="rshinystatistics"), paste0(path(),'/data/LICENSE'), overwrite = T)
        file.copy(system.file("templates/data", "LICENSE", package="rshinystatistics"), paste0(path(),'/data/LICENSE.txt'), overwrite = T)

        # ... saving code
        cat(hypothesisAsFile('Rmd', test, backup, dvs, between, covar), file = paste0(path(),'/code/',test,'.Rmd'))
        knitr::purl(paste0(path(),'/code/',test,'.Rmd'), paste0(path(),'/code/',test,'.R'), documentation = 2)
        file.copy(system.file("templates/code", "LICENSE", package="rshinystatistics"), paste0(path(),'/code/LICENSE'), overwrite = T)
        file.copy(system.file("templates/code", "LICENSE", package="rshinystatistics"), paste0(path(),'/code/LICENSE.txt'), overwrite = T)
        cat(as.character(
          templates::tmpl(paste(readLines(system.file("templates/code", "run", package="rshinystatistics")), collapse="\n"), test = test)
        ), file = paste0(path(),'/code/run'))

        # ... saving environment
        file.copy(system.file("templates/environment", "Dockerfile", package="rshinystatistics"), paste0(path(),'/environment/Dockerfile'), overwrite = T)

        # ... saving metadata
        str.formula <- as_formula(paste0(rdvs(), collapse = ','), between = rbetween(), covar = rcovar(), as.character = T)
        cat(as.character(
          templates::tmpl(paste(readLines(system.file("templates/metadata", "metadata.yml", package="rshinystatistics")), collapse="\n")
                          , title.test = toupper(test), str.formula = str.formula
                          , author = input$author, email = input$email
                          , description = paste0("(@author = ", input$author, ", @email = ", input$email, ")"))
        ), file = paste0(path(),'/metadata/metadata.yml'))

        # ... saving REPRODUCING
        cat(as.character(
          templates::tmpl(paste(readLines(system.file("templates", "REPRODUCING", package="rshinystatistics")), collapse="\n")
                          , reportId = reportId())
        ), file = paste0(path(),'/REPRODUCING.md'))

        # ... generating reports
        for (nfile in input$files) {
          progress$inc(inc, detail = paste('Generating',nfile,'files for the report'))
          rmarkdown::render(paste0(path(),'/code/',test,'.Rmd'), paste0(nfile,'_document'), output_dir = paste0(path(),'/results'))
        }

      }

      # .. button to generate and download the report

      observeEvent(input[[paste0("export",test,reportId())]], {
        if (!dataset$isSetup) return(NULL)
        validate(
          need(!is.null(dataset[[paste0(test,'Params')]][["hypothesis"]]),
               tl(paste0("Please perform the ",test," test before to generate the report files")))
        )
        progress <- shiny::Progress$new()
        progress$set(message = tl("Making report files"), value = 0)

        updateFiles(progress)
        on.exit(progress$close())

        output$renderSelectedFile <- renderPrint({
          list.files(path(), include.dirs = T, recursive = T)
        })
      })

      output$exportButtonUI <- renderUI({
        if (!dataset$isSetup) return(NULL)
        actionButton(ns(paste0("export",test,reportId())), tl("Generate Export Files"), icon = icon('file-export'))
      })

      output$downloadButtonUI <- renderUI({
        if (!is.null(input[[paste0("export",test,reportId())]]) && (input[[paste0("export",test,reportId())]]) > 0) {
          downloadButton(ns('downloadZIP'), tl('Download ZIP'))
        }
      })

      output$downloadZIP <- downloadHandler(
        filename = function() {
          str.formula <- as_formula(paste0(rdvs(), collapse = '_'), between = rbetween(), covar = rcovar(), as.character = T)
          paste0(ns("backup"), '-', URLencode(gsub('/','_',gsub('`','',str.formula))), '-', reportId(), ".zip")
        },
        content = function(file) { zip::zipr(file, files = paste0(path(),"/")) }
      )

    }
  )
}
