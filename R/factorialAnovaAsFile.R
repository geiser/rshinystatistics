
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

anova_plots_code <- function(backup, dataname, dvs, between, ext = 'Rmd') {
  anova.plots <- paste0(lapply(dvs, FUN = function(dv) {
    width <- 700
    height <- 700
    font.label.size <- 14
    step.increase <- 0.25
    addParam <- c("jitter")
    plot.param <- backup$anovaParams$plot[[dv]]
    if (!is.null(plot.param)) {
      addParam <- plot.param$addParam
      step.increase <- plot.param$step.increase
      font.label.size <- plot.param$font.label.size
      width <- plot.param$width
      height <- plot.param$height
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
      '), font.label.size=',font.label.size,', step.increase=',step.increase,')')
    if (ext == 'Rmd') {
      plot.code <- paste0(c("```{r}", plot.code, "```"), collapse = "\n")
    }
    plot.code <- paste0(plot.code,'\n',display.plots.str(ext,between, width=width, height=height, dv=dv))

    return(paste0('\n### Anova plots for the dependent variable "',dv,'"\n',plot.code,'\n'))
  }), collapse = "\n")
  return(anova.plots)
}


#' @export
factorialAnovaSummaryAsFile <- function(ext, backup, dvs = 'dvs', between = 'between', path = getwd(), lang = 'en') {

  wid <- backup$variables$wid
  rdvs <- unique(unlist(backup$variables[c(dvs)], use.names = F))
  rbetween <- unique(unlist(backup$variables[c(between)], use.names = F))

  code.skewness <- paste0(lapply(rdvs, FUN = function(dv) {
    line.code <- skewness_code(backup$skewness[[dv]], paste0('"',dv,'"'), paste0('dat[["',dv,'"]]'), paste0('rdat[["',dv,'"]]'))
    if (is.null(line.code)) return("")
    line.code <- paste0(c(
      paste0('density_res_plot(rdat[["',dv,'"]],"',dv,'",between)'),
      line.code,
      paste0('density_res_plot(rdat[["',dv,'"]],"',dv,'",between)')),
      collapse = "\n")
    if (ext == 'Rmd') {
      line.code <- paste0("\n```{r}\n",line.code,"\n```\n", "\n")
    }
    if (lang=='pt')
      return(paste0('\n Aplicando transformação in "',dv,'" para reduzir distorsão\n',line.code))
    else
      return(paste0('\n Applying transformation in "',dv,'" to reduce skewness\n',line.code))
  }), collapse = "\n")

  aov.params <- backup$anovaParams$hypothesis
  tfile <- system.file("templates", paste0("factorialAnova",ifelse(lang!='en',paste0('-',lang),''),".",ext), package="rshinystatistics")

  code.outliers <- ''
  if (backup$outlier.method == 'remove') {
    code.outliers <- list.as.code(backup$outliers)
  } else if (backup$outlier.method == 'winsorize') {
    code.outliers <- do.call(paste0, lapply(rdvs, FUN = function(dv) {
      paste0('rdat[["',dv,'"]] <- winzorize(rdat[["',dv,'"]],"',dv,'", c(',paste0(paste0('"',rbetween,'"'),collapse=','),')',')\n')
    }))
    if (ext == "Rmd") {
      code.outliers <- paste0(c("```{r}", code.outliers, "```"), collapse = '\n')
    }
  }

  params <- list(
    rshinystatistics.version = as.character(packageVersion("rshinystatistics")),
    author = backup$author, email = backup$email,
    wid = wid, dvs = rdvs, between = rbetween,
    code.outliers =  code.outliers,
    code.skewness = code.skewness,
    code.non.normal = list.as.code(backup$toRemoveForNormality),
    anova.plots = anova_plots_code(backup, 'sdat', rdvs, rbetween, ext),
    type = aov.params$type, effect.size = aov.params$effect.size,
    p.adjust.method = aov.params$p.adjust.method
  )

  if (ext != "Rmd") {
    params[["path"]] <- path
  } else {
    anova.text <- factorial.anova.as.text(backup$aov, backup$dataTable, rbetween, aov.params$effect.size, lang=lang)
    anova.pwc.text <- aov.pwc.as.text(backup$pwc, backup$ds, rbetween, p.adjust.method = aov.params$p.adjust.method, lang=lang)
    params[["anova.text"]] <- anova.text
    params[["anova.pwc.text"]] <- anova.pwc.text
  }

  return(as.character(
    do.call(templates::tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), params))
  ))
}


