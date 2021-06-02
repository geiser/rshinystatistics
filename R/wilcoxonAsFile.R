
wilcoxon_plots_code <- function(backup, dataname, dvs, iv, ext = 'Rmd') {
  wtest.plots <- paste0(lapply(dvs, FUN = function(dv) {
    width <- 700
    height <- 700
    font.label.size <- 14
    addParam <- "jitter"
    plot.param <- backup$wilcoxonParams$plot[[dv]]
    if (!is.null(plot.param)) {
      addParam <- plot.param$addParam
      font.label.size <- plot.param$font.label.size
      width <- plot.param$width
      height <- plot.param$height
    }
    plot.code <- paste0('ggPlotWilcoxon(',dataname,'[["',dv,'"]], "',iv,'", "',dv,'"',"\n",
                        ', res$wt[["',dv,'"]], c(',paste0(paste0('"',addParam,'"'),collapse =','),'), font.label.size=',font.label.size,')')
    if (ext == 'Rmd') {
      plot.code <- paste0("```{r, fig.width=", ceiling(width/100),", fig.height=",ceiling(height/100), "}\n", plot.code,"\n```\n")
    }

    return(paste0('\n### Wilcoxon test plot for the dependent variable "',dv,'"\n',plot.code,'\n'))
  }), collapse = "\n")
  return(wtest.plots)
}


#' @export
wilcoxonSummaryAsFile <- function(ext, backup, dvs = 'dvs', iv = 'iv', path = getwd(), lang='en') {

  wid <- backup$variables$wid
  rdvs <- unique(unlist(backup$variables[c(dvs)], use.names = F))
  riv <- unique(unlist(backup$variables[c(iv)], use.names = F))

  wtest.params <- backup$wilcoxonParams$hypothesis
  tfile <- system.file("templates", paste0("wilcoxonSummary",ifelse(lang!='en',paste0('-',lang),''),".",ext), package="rshinystatistics")
  params <- list(
    rshinystatistics.version = as.character(packageVersion("rshinystatistics")),
    author = backup$author, email = backup$email,
    wid = wid, dvs = rdvs, iv = riv,
    code.outliers =  list.as.code(backup$outliers),
    wtest.plots = wilcoxon_plots_code(backup, 'sdat', rdvs, riv, ext),
    alternative = wtest.params$alternative
  )
  if (ext != "Rmd") {
    params[["path"]] <- path
  } else {
    wilcoxon.text <- wilcoxon.as.text(backup$wilcoxon.test, backup$ds, riv, lang=lang)
    params[["wilcoxon.text"]] <- wilcoxon.text
  }
  return(as.character(
    do.call(templates::tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), params))
  ))
}


#' @export
wilcoxonDetailAsFile <- function(ext, backup, dv, iv = 'iv', path = getwd()) {

  wid <- backup$variables$wid
  riv <- unique(unlist(backup$variables[c(iv)], use.names = F))

  width <- 700
  height <- 700
  font.label.size <- 14
  addParam <- c("jitter")
  plot.param <- backup$wilcoxonParams$plot[[dv]]
  if (!is.null(plot.param)) {
    addParam <- plot.param$addParam
    font.label.size <- plot.param$font.label.size
    width <- plot.param$width
    height <- plot.param$height
  }

  wtest.params <- backup$wilcoxonParams$hypothesis
  tfile <- system.file("templates", paste0("wilcoxonDetail.",ext), package="rshinystatistics")
  params <- list(
    rshinystatistics.version = as.character(packageVersion("rshinystatistics")),
    author = backup$author, email = backup$email,
    wid = wid, dv = dv, iv = riv,
    outlier.ids =  backup$outliers[[dv]],
    addParam = addParam, font.label.size = font.label.size, width = width, height = height,
    alternative = wtest.params$alternative
  )
  if (ext != "Rmd") params[["path"]] <- path
  return(as.character(
    do.call(templates::tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), params))
  ))
}
