

#' Code for ANCOVA Plot
ancova.as.code.plots <- function(backup, dataname, dvs, between, covar, ext = 'Rmd') {
  ancova.plots <- paste0(lapply(dvs, FUN = function(dv) {
    width <- 700
    height <- 700
    font.label.size <- 14
    step.increase <- 0.25
    addParam <- c("jitter")
    plot.param <- backup$ancovaParams$plot[[dv]]
    if (!is.null(plot.param)) {
      addParam <- plot.param$addParam
      step.increase <- plot.param$step.increase
      font.label.size <- plot.param$font.label.size
      width <- plot.param$width
      height <- plot.param$height
      p.label <- plot.param$p.label
    }

    if (length(between) == 2) {
      nfunction <- 'twoWayAncovaPlots'
    } else if (length(between) == 1) {
      nfunction <- 'oneWayAncovaPlots'
    }

    plot.code <- paste0(
      'plots <- ', nfunction,'(',dataname,'[["',dv,'"]], "',dv,'", between',"\n",
      ', aov[["',dv,'"]], pwc[["',dv,'"]], addParam = c(',paste0(paste0('"',addParam,'"'), collapse = ','),')',
      ', font.label.size=',font.label.size,', step.increase=',step.increase,', p.label="',p.label,'"',')')
    if (ext == 'Rmd') {
      plot.code <- paste0(c("```{r}", plot.code, "```"), collapse = "\n")
    }

    plot.code <- paste0(c(plot.code, paste0(lapply(between, FUN = function(iv) {
      plot.inner.code <- paste0('plots[["',iv,'"]]')
      if (ext == 'Rmd') {
        plot.inner.code <- paste0(
          c(paste0("```{r, fig.width=", ceiling(width/100),", fig.height=", ceiling(height/100), "}"),
            plot.inner.code, "```"), collapse = "\n")
      }
      return(paste0('\n#### Plot for: `',dv,'` ~ `',iv,'`','\n',plot.inner.code,'\n'))
    }), collapse = "\n")), collapse = "\n")

    return(paste0('\n### Ancova plots for the dependent variable "',dv,'"\n',plot.code,'\n'))
  }), collapse = "\n")
  return(ancova.plots)
}
