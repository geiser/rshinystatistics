
#' Code for Kruskal-Wallis Plot
kruskal.as.code.plots <- function(backup, dataname, dvs, between, ext = 'Rmd') {
  kruskal.plots <- paste0(lapply(dvs, FUN = function(dv) {
    width <- 700
    height <- 700
    font.label.size <- 14
    step.increase <- 0.25
    addParam <- c("jitter")
    plot.param <- backup$kruskalParams$plot[[dv]]
    if (!is.null(plot.param)) {
      addParam <- plot.param$addParam
      step.increase <- plot.param$step.increase
      font.label.size <- plot.param$font.label.size
      width <- plot.param$width
      height <- plot.param$height
      p.label <- plot.param$p.label
    }

    plot.code <- paste0(
      'plots <- oneWayNonParamFactPlots(',dataname,'[["',dv,'"]], "',dv,'", between',"\n",
      ', kruskal[["',dv,'"]]$kt, pwc[["',dv,'"]], c(',paste0(paste0('"',addParam,'"'), collapse = ','),
      '), font.label.size=',font.label.size,', step.increase=',step.increase,', p.label="',p.label,'")')
    if (ext == 'Rmd') {
      plot.code <- paste0(c("```{r}", plot.code, "```"), collapse = "\n")
    }
    plot.code <- paste0(plot.code,'\n',display.plots.str(ext, between, width=width, height=height, dv=dv))

    return(paste0('\n### Kruskal-Wallis plots for the dependent variable "',dv,'"\n',plot.code,'\n'))
  }), collapse = "\n")
  return(kruskal.plots)
}

