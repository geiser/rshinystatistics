
list.as.code <- function(vlist) {
  code.out <- paste0(unlist(lapply(names(vlist), FUN = function(rvar) {
    ids <- as.character(vlist[[rvar]])
    if (length(ids) > 0){
      code.line <- paste0('"',rvar,'"'," = c(", paste0(paste0('"',ids,'"'), collapse=','),")")
      return(code.line)
    } else return(NULL)
  })), collapse = ",\n")
  paste0("list(\n", code.out, "\n)")
}

skewness_code <- function(skew, dvname, initTable='dat', dataTable = 'rdat') {
  if (skew %in% c('posSqrt','negSqrt','posLog','negLog','posInv','negInv')) {
    if (skew == 'posSqrt') {
      skewness.code <- paste0('sqrt(',initTable,'[[',dvname,']])')
    } else if (skew == 'negSqrt') {
      skewness.code <- paste0('sqrt(max(',initTable,'[[',dvname,']]+1) - ',initTable,'[[',dvname,']])')
    } else if (skew == 'posLog') {
      skewness.code <- paste0('log10(',initTable,'[[',dvname,']])')
    } else if (skew == 'negLog') {
      skewness.code <- paste0('log10(max(',initTable,'[[',dvname,']]+1) - ',initTable,'[[',dvname,']])')
    } else if (skew == 'posInv') {
      skewness.code <- paste0('1/(',initTable,'[[',dvname,']])')
    } else  if (skew == 'negInv') {
      skewness.code <- paste0('1/(max(',initTable,'[[',dvname,']]+1) - ',initTable,'[[',dvname,']])')
    }
    skewness.code <- paste0(dataTable,'[[',dvname,']] <- ', skewness.code)
    return(skewness.code)
  } else return(NULL)
}


#' Code for Wilcoxon Signed-Rank Plot
#'
#' @export
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

#' Code for Kruskal-Wallis Plot
#'
#' @export
kruskal_plots_code <- function(backup, dataname, dvs, between, ext = 'Rmd') {
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



#' Code for Scheirer-Ray-Hare Plot
#'
#' @export
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


#' Code for ANCOVA Plot
#'
#' @export
ancova_plots_code <- function(backup, dataname, dvs, between, covar, ext = 'Rmd') {
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
    }

    if (length(between) == 2) {
      nfunction <- 'twoWayAncovaPlots'
    } else if (length(between) == 1) {
      nfunction <- 'oneWayAncovaPlots'
    }

    plot.code <- paste0(
      'plots <- ', nfunction,'(',dataname,'[["',dv,'"]], "',dv,'", between',"\n",
      ', aov[["',dv,'"]], pwc[["',dv,'"]], addParam = c(',paste0(paste0('"',addParam,'"'), collapse = ','),')',
      ', font.label.size=',font.label.size,', step.increase=',step.increase,')')
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

#' Code for Linearity Assessment Plot
#'
#' @export
linearity_code <- function(backup, dataname, dvname, covarname, ivsnames, ext = "Rmd") {
  lmethod <- backup$lmethod
  linearity.code <- paste0(
    'ggscatter(',dataname,', x=',covarname,', y=',dvname,', facet.by=',ivsnames,', short.panel.labs = F) + \n stat_smooth(method = "',lmethod,'", span = 0.9)')
  if (ext == "Rmd") {
    linearity.code <- paste0("\n```{r}\n", linearity.code, "\n```\n")
    linearity.code <- paste0('\n### Assumption: Linearity of dependent variables and covariate variable \n', linearity.code,'\n')
  }
  linearity.code <- paste0(linearity.code, "\n")
  return(linearity.code)
}



