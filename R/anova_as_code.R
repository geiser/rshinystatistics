
#' Code for ANOVA Plot
anova.as.code.plots <- function(backup, dataname, dvs, between, ext = 'Rmd') {
  anova.plots <- paste0(lapply(dvs, FUN = function(dv) {
    width <- 700
    height <- 700
    font.label.size <- 14
    step.increase <- 0.25
    addParam <- c("jitter")
    p.label <- "p.adj.signif"
    plot.param <- backup$anovaParams$plot[[dv]]
    if (!is.null(plot.param)) {
      addParam <- plot.param$addParam
      step.increase <- plot.param$step.increase
      font.label.size <- plot.param$font.label.size
      width <- plot.param$width
      height <- plot.param$height
      p.label <- plot.param$p.label
    }

    if (length(between) == 1) {
      nfunction <- 'oneWayAnovaPlots'
    } else if (length(between) == 2) {
      nfunction <- 'twoWayAnovaPlots'
    } else if (length(between) == 3) {
      nfunction <- 'threeWayAnovaPlots'
    }

    plot.code <- paste0(
      'plots <- ', nfunction,'(',dataname,'[["',dv,'"]], "',dv,'", between',
      ', aov[["',dv,'"]], pwc[["',dv,'"]], c(',paste0(paste0('"',addParam,'"'), collapse = ','),
      '), font.label.size=',font.label.size,', step.increase=',step.increase,', p.label="',p.label,'"',')')
    if (ext == 'Rmd') {
      plot.code <- paste0(c("```{r, dpi=300}", plot.code, "```"), collapse = "\n")
    }
    plot.code <- paste0(plot.code,'\n',display.plots.str(ext,between, width=width, height=height, dv=dv))

    return(paste0('\n### Anova plots for the dependent variable "',dv,'"\n',plot.code,'\n'))
  }), collapse = "\n")
  return(anova.plots)
}
