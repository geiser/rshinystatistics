
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


display.plots.str <- function(ext, ivs, width=700, height=700, dv='dependent variable') {
  plot.code <- ''
  if (length(ivs) == 3) {
    plot.code <- paste0(c(plot.code, paste0(lapply(ivs, FUN = function(iv) {
      grpbys <- setdiff(ivs,iv)
      paste0(lapply(grpbys, FUN = function(grpby) {
        color <- setdiff(ivs, c(iv,grpby))
        plot.inner.code <- paste0('plots[["',iv,'"]][["',grpby,'"]]')
        if (ext == 'Rmd') {
          plot.inner.code <- paste0(
            c(paste0("```{r, fig.width=", ceiling(width/100),", fig.height=", ceiling(height/100), "}"),
              plot.inner.code, "```"), collapse = "\n")
          plot.inner.code <- paste0('\n#### Plot of "',dv,'" based on "',iv,'" grouped by "',grpby,'" (color: ', color,')\n',plot.inner.code,'\n')
        }
        return(paste0(plot.inner.code,'\n'))
      }), collapse = '\n')
    }))), collapse = '\n')
  } else if (length(ivs) < 3) {
    plot.code <- paste0(c(plot.code, paste0(lapply(ivs, FUN = function(iv) {
      plot.inner.code <- paste0('plots[["',iv,'"]]')
      col.str <- ''
      if (length(setdiff(ivs,iv)) > 0) {
        col.str <- paste0(' (color: ',setdiff(ivs,iv),')')
      }
      if (ext == 'Rmd') {
        plot.inner.code <- paste0(
          c(paste0("```{r, fig.width=", ceiling(width/100),", fig.height=", ceiling(height/100), "}"),
            plot.inner.code, "```"), collapse = "\n")
      }
      return(paste0('\n#### Plot of "',dv,'" based on "',iv,'"',col.str,'\n',plot.inner.code,'\n'))
    }), collapse = "\n")), collapse = "\n")
  }
  return(plot.code)
}


skewness.as.code <- function(skew, dvname, initTable='dat', dataTable = 'rdat') {
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

#' Code for Linearity Assessment Plot
#'
#' @export
linearity.as.code <- function(backup, dataname, dvname, covarname, ivsnames, ext = "Rmd") {
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


