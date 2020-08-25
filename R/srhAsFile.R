
srh_plots_code <- function(backup, dataname, dvs, between, ext = 'Rmd') {
  srh.plots <- paste0(lapply(dvs, FUN = function(dv) {
    width <- 800
    height <- 600
    font.label.size <- 10
    step.increase <- 0.005
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
      'plots <- ', nfunction,'(',dataname,'[which(',dataname,'[["var"]] == "',dv,'"),], "',dv,'", between',"\n",
      ', srh[["',dv,'"]], pwc[["',dv,'"]], c(',paste0(paste0('"',addParam,'"'), collapse = ','),
      '), font.label.size=',font.label.size,', step.increase=',step.increase,', type = "srh")')
    if (ext == 'Rmd') {
      plot.code <- paste0(c("```{r echo=FALSE}", plot.code, "```"), collapse = "\n")
    }
    plot.code <- paste0(plot.code,'\n',display.plots.str(ext, between, width=width, height=height, dv=dv))

    return(paste0('\n### Scheirer-Ray-Hare plots for the dependent variable "',dv,'"\n',plot.code,'\n'))
  }), collapse = "\n")
  return(srh.plots)
}


#' @export
srhSummaryAsFile <- function(ext, backup, dvs = 'dvs', between = 'between', path = getwd()) {

  wid <- backup$variables$wid
  rdvs <- unique(unlist(backup$variables[c(dvs)], use.names = F))
  rbetween <- unique(unlist(backup$variables[c(between)], use.names = F))

  srh.params <- backup$srhParams$hypothesis
  tfile <- system.file("templates", paste0("srhSummary.",ext), package="rshinystatistics")

  params <- list(
    rshinystatistics.version = as.character(packageVersion("rshinystatistics")),
    author = backup$author, email = backup$email,
    wid = wid, dvs = rdvs, between = rbetween,
    code.outliers =  list.as.code(backup$outliers),
    srh.plots = srh_plots_code(backup, 'sdat', rdvs, rbetween, ext),
    pwc.method = srh.params$pwc.method,
    p.adjust.method = srh.params$p.adjust.method
  )

  if (ext != "Rmd") {
    params[["path"]] <- path
  } else {
    #anova.text <- factorial.anova.as.text(backup$aov, backup$dataTable, rbetween, aov.params$effect.size)
    #anova.pwc.text <- factorial.anova.pwc.as.text(backup$pwc, rbetween, p.adjust.method = aov.params$p.adjust.method)
    #params[["anova.text"]] <- anova.text
    #params[["anova.pwc.text"]] <- anova.pwc.text
  }
  return(as.character(
    do.call(templates::tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), params))
  ))
}


#' @export
srhDetailAsFile <- function(ext, backup, dv, between = 'between', path = getwd()) {

  wid <- backup$variables$wid
  rbetween <- unique(unlist(backup$variables[c(between)], use.names = F))

  width <- 800
  height <- 600
  font.label.size <- 10
  step.increase <- 0.005
  addParam <- c("jitter")
  plot.param <- backup$srhParams$plot[[dv]]
  if (!is.null(plot.param)) {
    addParam <- plot.param$addParam
    step.increase <- plot.param$step.increase
    font.label.size <- plot.param$font.label.size
    width <- plot.param$width
    height <- plot.param$height
  }

  fname <- 'oneWayNonParamFactPlots'
  if (length(rbetween) == 2) fname <- 'twoWayNonParamFactPlots'
  if (length(rbetween) == 3) fname <- 'threeWayNonParamFactPlots'
  plot.code <- paste0('plots <- ',fname,'(sdat,"',dv,'",c(',paste0(paste0('"',rbetween,'"'),collapse=','),'),\n',
                      'srh[["',dv,'"]],pwc[["',dv,'"]],addParam=c(',paste0(paste0('"',addParam,'"'), collapse=','),'),',
                      'font.label.size=',font.label.size,',step.increase=',step.increase,', type = "srh")')

  if (ext == 'Rmd') {
    plot.code <- paste0(c("```{r echo=FALSE}", plot.code, "```"), collapse = "\n")
  }
  plot.code <- paste0(plot.code,'\n',display.plots.str(ext,rbetween, width=width, height=height, dv=dv))

  srh.params <- backup$srhParams$hypothesis
  tfile <- system.file("templates", paste0("srhDetail.",ext), package="rshinystatistics")
  params <- list(
    rshinystatistics.version = as.character(packageVersion("rshinystatistics")),
    author = backup$author, email = backup$email,
    wid = wid, dv = dv, between = rbetween,
    outlier.ids =  backup$outliers[[dv]],
    srh.plots = plot.code,
    pwc.method = srh.params$pwc.method,
    p.adjust.method = srh.params$p.adjust.method
  )
  if (ext != "Rmd") {
    params[["path"]] <- path
  } else {
    #anova.text <- factorial.anova.as.text(backup$aov, backup$dataTable, rbetween, aov.params$effect.size)
    #anova.pwc.text <- factorial.anova.pwc.as.text(backup$pwc, rbetween, p.adjust.method = aov.params$p.adjust.method)
    #params[["anova.text"]] <- anova.text
    #params[["anova.pwc.text"]] <- anova.pwc.text
  }
  return(as.character(
    do.call(templates::tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), params))
  ))
}

