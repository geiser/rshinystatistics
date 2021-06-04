
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
wilcoxonAsFile <- function(ext, backup, dvs = 'dvs', iv = 'iv', path = getwd(), lang='en') {

  wid <- backup$variables$wid
  rdvs <- unique(unlist(backup$variables[c(dvs)], use.names = F))
  riv <- unique(unlist(backup$variables[c(iv)], use.names = F))

  code.skewness <- paste0(lapply(rdvs, FUN = function(dv) {
    line.code <- skewness_code(backup$skewness[[dv]], paste0('"',dv,'"'), paste0('dat[["',dv,'"]]'), paste0('rdat[["',dv,'"]]'))
    if (is.null(line.code)) return("")
    line.code <- paste0(c(
      paste0('density_res_plot(rdat[["',dv,'"]],"',dv,'",between)'),
      line.code,
      paste0('density_res_plot(rdat[["',dv,'"]],"',dv,'",between)')),
      collapse = "\n")
    if (ext == 'Rmd') {
      line.code <- paste0("\n```{r}\n",line.code,"\n```\n", "\n")
    }
    if (lang=='pt')
      return(paste0('\n Aplicando transformação in "',dv,'" para reduzir distorsão\n',line.code))
    else
      return(paste0('\n Applying transformation in "',dv,'" to reduce skewness\n',line.code))
  }), collapse = "\n")

  wilcoxon.params <- backup$wilcoxonParams$hypothesis
  tfile <- system.file("templates", paste0("wilcoxon",ifelse(lang!='en',paste0('-',lang),''),".",ext), package="rshinystatistics")

  code.outliers <- ''
  if (backup$outlier.method == 'remove') {
    code.outliers <- list.as.code(backup$outliers)
  } else if (backup$outlier.method == 'winsorize') {
    code.outliers <- do.call(paste0, lapply(rdvs, FUN = function(dv) {
      paste0('rdat[["',dv,'"]] <- winzorize(rdat[["',dv,'"]],"',dv,'", c(',paste0(paste0('"',riv,'"'),collapse=','),')',')\n')
    }))
    if (ext == "Rmd") {
      code.outliers <- paste0(c("```{r}", code.outliers, "```"), collapse = '\n')
    }
  }

  params <- list(
    rshinystatistics.version = as.character(packageVersion("rshinystatistics")),
    author = backup$author, email = backup$email,
    wid = wid, dvs = rdvs, iv = riv,
    code.outliers =  code.outliers,
    code.skewness = code.skewness,
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


