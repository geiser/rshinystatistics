
ind_ttest_plots_code <- function(backup, dataname, dvs, iv, ext = 'Rmd') {
  ttest.plots <- paste0(lapply(dvs, FUN = function(dv) {
    width <- 800
    height <- 600
    font.label.size <- 12
    addParam <- "jitter"
    plot.param <- backup$indSampleTTestParams$plot[[dv]]
    if (!is.null(plot.param)) {
      addParam <- plot.param$addParam
      font.label.size <- plot.param$font.label.size
      width <- plot.param$width
      height <- plot.param$height
    }

    plot.code <- paste0('ggPlotTTest(',dataname,'[["',dv,'"]], "',iv,'", "',dv,'", res$tt[["',dv,'"]], c(',paste0(paste0('"',addParam,'"'),collapse =','),'), font.label.size=',font.label.size,')')
    if (ext == 'Rmd') {
      plot.code <- paste0("```{r, fig.width=", ceiling(width/100),", fig.height=",ceiling(height/100), "}\n", plot.code,"\n```\n")
    }

    return(paste0('\n### T-Test plot for the dependent variable "',dv,'"\n',plot.code,'\n'))
  }), collapse = "\n")
  return(ttest.plots)
}


#' @export
ind.ttestSummaryAsFile <- function(ext, backup, dvs = 'dvs', iv = 'iv', path = getwd(), lang = 'en') {

  wid <- backup$variables$wid
  rdvs <- unique(unlist(backup$variables[c(dvs)], use.names = F))
  riv <- unique(unlist(backup$variables[c(iv)], use.names = F))

  code.skewness <- paste0(lapply(rdvs, FUN = function(dv) {
    line.code <- skewness_code(paste0('rdat[["',dv,'"]]'), backup$skewness[[dv]], paste0('"',dv,'"'))
    if (is.null(line.code)) return("")
    line.code <- paste0(c(
      paste0('density_res_plot(rdat,"',dv,'",iv,dv.var="var")'),
      line.code,
      paste0('density_res_plot(rdat,"',dv,'",iv,dv.var="var")')),
      collapse = "\n")
    if (ext == 'Rmd') {
      line.code <- paste0("\n```{r}\n",line.code,"\n```\n", "\n")
    }
    if (lang == 'pt')
      return(paste0('\n##### Aplicando transformação na ',dv,' para reduzir a distorção\n',line.code))
    else
      return(paste0('\n##### Applying transformation in ',dv,' to reduce skewness\n',line.code))
  }), collapse = "\n")

  ttest.params <- backup$indSampleTTestParams$hypothesis
  tfile <- system.file("templates", paste0("ind.ttestSummary",ifelse(lang!='en',paste0('-',lang),''),".",ext), package="rshinystatistics")
  params <- list(
    rshinystatistics.version = as.character(packageVersion("rshinystatistics")),
    author = backup$author, email = backup$email,
    wid = wid, dvs = rdvs, iv = riv,
    code.outliers =  list.as.code(backup$outliers),
    code.skewness = code.skewness,
    code.non.normal = list.as.code(backup$toRemoveForNormality),
    ttest.plots = ind_ttest_plots_code(backup, 'sdat', rdvs, riv, ext),
    alternative = ttest.params$alternative, var.equal = ttest.params$var.equal,
    hedges.correction = ttest.params$hedges.correction
  )
  if (ext != "Rmd") {
    params[["path"]] <- path
  } else {
    ttest.text <- ttest.as.text(backup$t.test, backup$t.test.ds, riv, ttest.params$var.equal, ttest.params$hedges.correction, lang = lang)
    params[["ttest.text"]] <- ttest.text
  }
  return(as.character(
    do.call(templates::tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), params))
  ))
}


#' @export
ind.ttestDetailAsFile <- function(ext, backup, dv, iv = 'iv', path = getwd()) {

  wid <- backup$variables$wid
  riv <- unique(unlist(backup$variables[c(iv)], use.names = F))

  code.skewness <- ""
  line.code <- skewness_code('rdat', backup$skewness[[dv]], paste0('"',dv,'"'))
  if (!is.null(line.code)) {
    line.code <- paste0(c(
      paste0('density_grp_plot(rdat, "',dv,'", "',wid,'", rdat[["',wid,'"]])'),
      line.code,
      paste0('density_grp_plot(rdat, "',dv,'", "',wid,'", rdat[["',wid,'"]])')
    ), collapse = "\n")
    if (ext == 'Rmd') {
      line.code <- paste0("```{r}\n",line.code,"\n```\n", "\n")
    }
    code.skewness <- paste0('\n##### Applying normality in ',dv,' to reduce skewness\n',line.code)
  }

  width <- 700
  height <- 700
  font.label.size <- 12
  addParam <- c("jitter")
  plot.param <- backup$indSampleTTestParams$plot[[dv]]
  if (!is.null(plot.param)) {
    addParam <- plot.param$addParam
    font.label.size <- plot.param$font.label.size
    width <- plot.param$width
    height <- plot.param$height
  }

  ttest.params <- backup$indSampleTTestParams$hypothesis
  tfile <- system.file("templates", paste0("ind.ttestDetail.",ext), package="rshinystatistics")
  params <- list(
    rshinystatistics.version = as.character(packageVersion("rshinystatistics")),
    author = backup$author, email = backup$email,
    wid = wid, dv = dv, iv = riv,
    outlier.ids =  backup$outliers[[dv]],
    code.skewness = code.skewness,
    non.normal.ids = backup$toRemoveForNormality[[dv]],
    addParam = addParam, font.label.size = font.label.size, width = width, height = height,
    alternative = ttest.params$alternative,
    var.equal = ttest.params$var.equal,
    hedges.correction = ttest.params$hedges.correction
  )
  if (ext != "Rmd") params[["path"]] <- path
  return(as.character(
    do.call(templates::tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), params))
  ))
}




