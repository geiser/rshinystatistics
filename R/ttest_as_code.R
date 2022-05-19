
#' Code for independent t-Test Plot
ind.ttest.as.code.plots <- function(backup, dataname, dvs, iv, ext = 'Rmd') {
  ttest.plots <- paste0(lapply(dvs, FUN = function(dv) {
    width <- 700
    height <- 700
    font.label.size <- 14
    addParam <- c("jitter")
    p.label <- "p.adj.signif"
    plot.param <- backup$ttestParams$plot[[dv]]
    if (!is.null(plot.param)) {
      addParam <- plot.param$addParam
      font.label.size <- plot.param$font.label.size
      width <- plot.param$width
      height <- plot.param$height
    }

    plot.code <- paste0('ggPlotTTest(',dataname,'[["',dv,'"]], "',iv,'", "',dv,'", res$tt[["',dv,'"]], c(',paste0(paste0('"',addParam,'"'),collapse =','),'), font.label.size=',font.label.size,')')
    if (ext == 'Rmd') {
      plot.code <- paste0("```{r, dpi=300, fig.width=", ceiling(width/100),", fig.height=",ceiling(height/100), "}\n", plot.code,"\n```\n")
    }

    return(paste0('\n### T-Test plot for the dependent variable "',dv,'"\n',plot.code,'\n'))
  }), collapse = "\n")
  return(ttest.plots)
}
