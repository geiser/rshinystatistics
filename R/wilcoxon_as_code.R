

#' Code for Wilcoxon Signed-Rank Plot
wilcoxon.as.code.plots <- function(backup, dataname, dvs, iv, ext = 'Rmd') {
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
      p.label <- plot.param$p.label
    }
    plot.code <- paste0('ggPlotWilcoxon(',dataname,'[["',dv,'"]], "',iv,'", "',dv,'"',"\n",
                        ', res$wt[["',dv,'"]], c(',paste0(paste0('"',addParam,'"'),collapse =','),'), font.label.size=',font.label.size,', p.label="',p.label,'")')
    if (ext == 'Rmd') {
      plot.code <- paste0("```{r, fig.width=", ceiling(width/100),", fig.height=",ceiling(height/100), "}\n", plot.code,"\n```\n")
    }

    return(paste0('\n### Wilcoxon test plot for the dependent variable "',dv,'"\n',plot.code,'\n'))
  }), collapse = "\n")
  return(wtest.plots)
}

