
kruskal_plots_code <- function(backup, dataname, dvs, between, ext = 'Rmd') {
  kruskal.plots <- paste0(lapply(dvs, FUN = function(dv) {
    width <- 800
    height <- 600
    font.label.size <- 10
    step.increase <- 0.005
    addParam <- c("jitter")
    plot.param <- backup$kruskalParams$plot[[dv]]
    if (!is.null(plot.param)) {
      addParam <- plot.param$addParam
      step.increase <- plot.param$step.increase
      font.label.size <- plot.param$font.label.size
      width <- plot.param$width
      height <- plot.param$height
    }

    plot.code <- paste0(
      'plots <- oneWayNonParamFactPlots(',dataname,'[["',dv,'"]], "',dv,'", between',"\n",
      ', kruskal[["',dv,'"]]$kt, pwc[["',dv,'"]], c(',paste0(paste0('"',addParam,'"'), collapse = ','),
      '), font.label.size=',font.label.size,', step.increase=',step.increase,')')
    if (ext == 'Rmd') {
      plot.code <- paste0(c("```{r}", plot.code, "```"), collapse = "\n")
    }
    plot.code <- paste0(plot.code,'\n',display.plots.str(ext, between, width=width, height=height, dv=dv))

    return(paste0('\n### Kruskal-Wallis plots for the dependent variable "',dv,'"\n',plot.code,'\n'))
  }), collapse = "\n")
  return(kruskal.plots)
}


#' @export
kruskalSummaryAsFile <- function(ext, backup, dvs = 'dvs', between = 'between', path = getwd()) {

  wid <- backup$variables$wid
  rdvs <- unique(unlist(backup$variables[c(dvs)], use.names = F))
  rbetween <- unique(unlist(backup$variables[c(between)], use.names = F))

  kruskal.params <- backup$kruskalParams$hypothesis
  tfile <- system.file("templates", paste0("kruskalSummary.",ext), package="rshinystatistics")

  params <- list(
    rshinystatistics.version = as.character(packageVersion("rshinystatistics")),
    author = backup$author, email = backup$email,
    wid = wid, dvs = rdvs, between = rbetween,
    code.outliers =  list.as.code(backup$outliers),
    kruskal.plots = kruskal_plots_code(backup, 'sdat', rdvs, rbetween, ext),
    pwc.method = kruskal.params$pwc.method,
    p.adjust.method = kruskal.params$p.adjust.method
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
kruskalDetailAsFile <- function(ext, backup, dv, between = 'between', path = getwd()) {

  wid <- backup$variables$wid
  rbetween <- unique(unlist(backup$variables[c(between)], use.names = F))

  width <- 800
  height <- 600
  font.label.size <- 10
  step.increase <- 0.005
  addParam <- c("jitter")
  plot.param <- backup$kruskalParams$plot[[dv]]
  if (!is.null(plot.param)) {
    addParam <- plot.param$addParam
    step.increase <- plot.param$step.increase
    font.label.size <- plot.param$font.label.size
    width <- plot.param$width
    height <- plot.param$height
  }

  plot.code <- paste0('plots <- oneWayNonParamFactPlots(sdat,"',dv,'",c(',paste0(paste0('"',rbetween,'"'),collapse=','),'),\n',
                      'kruskal[["',dv,'"]]$kt,pwc[["',dv,'"]],addParam=c(',paste0(paste0('"',addParam,'"'), collapse=','),'),',
                      'font.label.size=',font.label.size,',step.increase=',step.increase,')')

  if (ext == 'Rmd') {
    plot.code <- paste0(c("```{r}", plot.code, "```"), collapse = "\n")
  }
  plot.code <- paste0(plot.code,'\n',display.plots.str(ext,rbetween, width=width, height=height, dv=dv))

  kruskal.params <- backup$kruskalParams$hypothesis
  tfile <- system.file("templates", paste0("kruskalDetail.",ext), package="rshinystatistics")
  params <- list(
    rshinystatistics.version = as.character(packageVersion("rshinystatistics")),
    author = backup$author, email = backup$email,
    wid = wid, dv = dv, between = rbetween,
    outlier.ids =  backup$outliers[[dv]],
    kruskal.plots = plot.code,
    pwc.method = kruskal.params$pwc.method,
    p.adjust.method = kruskal.params$p.adjust.method
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

