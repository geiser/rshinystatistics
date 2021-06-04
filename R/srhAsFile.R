
srh_plots_code <- function(backup, dataname, dvs, between, ext = 'Rmd') {
  srh.plots <- paste0(lapply(dvs, FUN = function(dv) {
    width <- 700
    height <- 700
    font.label.size <- 14
    step.increase <- 0.25
    addParam <- c("jitter")
    plot.param <- backup$srhParams$plot[[dv]]
    if (!is.null(plot.param)) {
      addParam <- plot.param$addParam
      step.increase <- plot.param$step.increase
      font.label.size <- plot.param$font.label.size
      width <- plot.param$width
      height <- plot.param$height
    }

    if (length(between) == 1) {
      nfunction <- 'oneWayNonParamFactPlots'
    } else if (length(between) == 2) {
      nfunction <- 'twoWayNonParamFactPlots'
    } else if (length(between) == 3) {
      nfunction <- 'threeWayNonParamFactPlots'
    }

    plot.code <- paste0(
      'plots <- ', nfunction,'(',dataname,'[["',dv,'"]], "',dv,'", between',
      ', srh[["',dv,'"]], pwc[["',dv,'"]], c(',paste0(paste0('"',addParam,'"'), collapse = ','),
      '), font.label.size=',font.label.size,', step.increase=',step.increase,', type = "srh")')
    if (ext == 'Rmd') {
      plot.code <- paste0(c("```{r}", plot.code, "```"), collapse = "\n")
    }
    plot.code <- paste0(plot.code,'\n',display.plots.str(ext, between, width=width, height=height, dv=dv))

    return(paste0('\n### Scheirer-Ray-Hare plots for the dependent variable "',dv,'"\n',plot.code,'\n'))
  }), collapse = "\n")
  return(srh.plots)
}


#' @export
srhAsFile <- function(ext, backup, dvs = 'dvs', between = 'between', path = getwd(), lang='en') {

  wid <- backup$variables$wid
  rdvs <- unique(unlist(backup$variables[c(dvs)], use.names = F))
  rbetween <- unique(unlist(backup$variables[c(between)], use.names = F))

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

  srh.params <- backup$srhParams$hypothesis
  tfile <- system.file("templates", paste0("srh",ifelse(lang!='en',paste0('-',lang),''),".",ext), package="rshinystatistics")

  code.outliers <- ''
  if (backup$outlier.method == 'remove') {
    code.outliers <- list.as.code(backup$outliers)
  } else if (backup$outlier.method == 'winsorize') {
    code.outliers <- do.call(paste0, lapply(rdvs, FUN = function(dv) {
      paste0('rdat[["',dv,'"]] <- winzorize(rdat[["',dv,'"]],"',dv,'", c(',paste0(paste0('"',rbetween,'"'),collapse=','),')',')\n')
    }))
    if (ext == "Rmd") {
      code.outliers <- paste0(c("```{r}", code.outliers, "```"), collapse = '\n')
    }
  }

  params <- list(
    rshinystatistics.version = as.character(packageVersion("rshinystatistics")),
    author = backup$author, email = backup$email,
    wid = wid, dvs = rdvs, between = rbetween,
    code.outliers =  code.outliers,
    code.skewness = code.skewness,
    srh.plots = srh_plots_code(backup, 'sdat', rdvs, rbetween, ext),
    pwc.method = srh.params$pwc.method,
    p.adjust.method = srh.params$p.adjust.method
  )

  if (ext != "Rmd") {
    params[["path"]] <- path
  } else {
    srh.text <- srh.as.text(backup$srh, backup$dataTable, rbetween, lang=lang)
    srh.pwc.text <- wilcoxon.pwc.as.text(backup$pwc, backup$ds, rbetween, p.adjust.method = srh.params$p.adjust.method, lang=lang)
    params[["srh.text"]] <- srh.text
    params[["srh.pwc.text"]] <- srh.pwc.text
  }
  return(as.character(
    do.call(templates::tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), params))
  ))
}


