
linearity_code <- function(dataname, dvname, covarname, ivsnames, ext = "Rmd") {
  linearity.code <- paste0(
    'ggscatter(',dataname,', x=',covarname,', y=',dvname,', facet.by=',ivsnames,', short.panel.labs = F) + \n stat_smooth(method = "loess", span = 0.9)')
  if (ext == "Rmd") {
    linearity.code <- paste0("* Linearity test in ", dvname,"\n ```{r}\n", linearity.code, "\n```\n")
  }
  linearity.code <- paste0(linearity.code, "\n")
  return(linearity.code)
}

ancova_plots_code <- function(backup, dataname, dvs, between, ext = 'Rmd') {
  ancova.plots <- paste0(lapply(dvs, FUN = function(dv) {
    width <- 900
    height <- 900
    font.label.size <- 14
    step.increase <- 0.25
    plot.param <- backup$ancovaParams$plot[[dv]]
    if (!is.null(plot.param)) {
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

    plot.code <- paste0('plots <- ', nfunction,'(',dataname,'[["',dv,'"]], "',dv,'", between',"\n",
                        ', aov[["',dv,'"]], pwc[["',dv,'"]], font.label.size=',font.label.size,', step.increase=',step.increase,')')
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


#' @export
ancovaSummaryAsFile <- function(ext, backup, dvs = 'dvs', between = 'between', covar = 'covar', path = getwd(), lang = 'en') {

  wid <- backup$variables$wid
  rdvs <- unique(unlist(backup$variables[c(dvs)], use.names = F))
  rbetween <- unique(unlist(backup$variables[c(between)], use.names = F))
  rcovar <- unique(unlist(backup$variables[c(covar)], use.names = F))


  code.skewness <- paste0(lapply(rdvs, FUN = function(dv) {
    line.code <- skewness_code(backup$skewness[[dv]], paste0('"',dv,'"'), paste0('dat[["',dv,'"]]'), paste0('rdat[["',dv,'"]]'))
    if (is.null(line.code)) return("")

    if (length(backup$skewness) > 0 && length(backup$skewness[[rcovar]]) > 0) {
      line.code <- paste0(
        line.code, '\n',
        skewness_code(backup$skewness[[rcovar]], paste0('"',rcovar,'"')
                      , initTable=paste0('dat[["',dv,'"]]'), dataTable=paste0('rdat[["',dv,'"]]'))
      )
    }

    line.code <- paste0(c(
      paste0('density_res_plot(rdat[["',dv,'"]],"',dv,'",between,c(),covar)'),
      line.code,
      paste0('density_res_plot(rdat[["',dv,'"]],"',dv,'",between,c(),covar)')),
      collapse = "\n")
    if (ext == 'Rmd') {
      line.code <- paste0("\n```{r}\n",line.code,"\n```\n", "\n")
    }

    if (lang=='pt')
      return(paste0('\n Aplicando transformação in "',dv,'" para reduzir distorsão\n',line.code))
    else
      return(paste0('\n Applying transformation in "',dv,'" to reduce skewness\n',line.code))
  }), collapse = "\n")

  ldvs <- as.list(rdvs); names(ldvs) <- rdvs
  linearity.code <- paste0(lapply(rdvs, FUN = function(dv) {
    line.code <- linearity_code(paste0('sdat[["',dv,'"]]'), paste0('"',dv,'"'), covar, between, ext)
    return(line.code)
  }), collapse = "\n")

  aov.params <- backup$ancovaParams$hypothesis
  tfile <- system.file("templates", paste0("ancova",ifelse(lang!='en',paste0('-',lang),''),".",ext), package="rshinystatistics")

  code.outliers <- ''
  if (backup$outlier.method == 'remove') {
    code.outliers <- list.as.code(backup$outliers)
  } else if (backup$outlier.method == 'winsorize') {
    code.outliers <- do.call(paste0, lapply(rdvs, FUN = function(dv) {
      paste0('rdat[["',dv,'"]] <- winzorize(rdat[["',dv,'"]],"',dv,'", c(',paste0(paste0('"',rbetween,'"'),collapse=','),')',',"',rcovar,'")\n')
    }))
    if (ext == "Rmd") {
      code.outliers <- paste0(c("```{r}", code.outliers, "```"), collapse = '\n')
    }
  }

  params <- list(
    rshinystatistics.version = as.character(packageVersion("rshinystatistics")),
    author = backup$author, email = backup$email,
    wid = wid, dvs = rdvs, between = rbetween, covar = rcovar,
    code.outliers =  code.outliers,
    code.skewness = code.skewness,
    code.non.normal = list.as.code(backup$toRemoveForNormality),
    linearity.code = linearity.code,
    ancova.plots = ancova_plots_code(backup, 'sdat', rdvs, rbetween, ext),
    type = aov.params$type, effect.size = aov.params$effect.size,
    p.adjust.method = aov.params$p.adjust.method
  )

  if (ext != "Rmd") {
    params[["path"]] <- path
  } else {
    ancova.text <- ancova.as.text(backup$aov, backup$dataTable, rbetween, rcovar, aov.params$effect.size, lang=lang)
    ancova.pwc.text <- aov.pwc.as.text(backup$pwc, backup$ds, rbetween, p.adjust.method = aov.params$p.adjust.method, lang=lang)
    params[["ancova.text"]] <- ancova.text
    params[["ancova.pwc.text"]] <- ancova.pwc.text
  }

  return(as.character(
    do.call(templates::tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), params))
  ))
}


#' @export
ancovaDetailAsFile <- function(ext, backup, dv, between = 'between', covar = 'covar', path = getwd()) {

  wid <- backup$variables$wid
  rbetween <- unique(unlist(backup$variables[c(between)], use.names = F))
  rcovar <- unique(unlist(backup$variables[c(covar)], use.names = F))

  code.skewness <- ""
  line.code <- skewness_code('rdat', backup$skewness[[dv]], paste0('"',dv,'"'))
  if (!is.null(line.code)) {
    line.code <- paste0(c(
      paste0('density_res_plot(rdat,"',dv,'", c(',paste0(paste0('"',rbetween,'"'), collapse = ","),'),c(),"',rcovar,'")'),
      line.code,
      paste0('density_res_plot(rdat,"',dv,'", c(',paste0(paste0('"',rbetween,'"'), collapse = ","),'),c(),"',rcovar,'")')
    ), collapse = "\n")
    if (ext == 'Rmd') {
      line.code <- paste0("```{r}\n",line.code,"\n```\n", "\n")
    }
    code.skewness <- paste0('\n##### Applying normality in ',dv,' to reduce skewness\n',line.code)
  }

  width <- 900
  height <- 900
  font.label.size <- 14
  step.increase <- 0.25
  plot.param <- backup$ancovaParams$plot[[dv]]
  if (!is.null(plot.param)) {
    step.increase <- plot.param$step.increase
    font.label.size <- plot.param$font.label.size
    width <- plot.param$width
    height <- plot.param$height
  }

  fname <- 'oneWayAncovaPlots'
  if (length(rbetween) == 2)
    fname <- 'twoWayAncovaPlots'
  plot.code <- paste0('plots <- ',fname,'(sdat,"',dv,'",c(',paste0(paste0('"',rbetween,'"'),collapse=','),'),\n',
                      'aov[["',dv,'"]],pwc[["',dv,'"]],font.label.size=',font.label.size,',step.increase=',step.increase,')')
  if (ext == 'Rmd') {
    plot.code <- paste0(c("```{r echo=FALSE}", plot.code, "```"), collapse = "\n")
  }
  plot.code <- paste0(c(plot.code, paste0(lapply(rbetween, FUN = function(iv) {
    plot.inner.code <- paste0('plots[["',iv,'"]]')
    if (ext == 'Rmd') {
      plot.inner.code <- paste0(
        c(paste0("```{r, fig.width=", ceiling(width/100),", fig.height=", ceiling(height/100), "}"),
          plot.inner.code, "```"), collapse = "\n")
    }
    return(paste0('\n#### Plot for: `',dv,'` ~ `',iv,'`','\n',plot.inner.code,'\n'))
  }), collapse = "\n")), collapse = "\n")


  aov.params <- backup$ancovaParams$hypothesis
  tfile <- system.file("templates", paste0("ancovaDetail.",ext), package="rshinystatistics")
  params <- list(
    rshinystatistics.version = as.character(packageVersion("rshinystatistics")),
    author = backup$author, email = backup$email,
    wid = wid, dv = dv, between = rbetween, covar = rcovar,
    outlier.ids =  backup$outliers[[dv]],
    code.skewness = code.skewness,
    non.normal.ids = backup$toRemoveForNormality[[dv]],
    ancova.plots = plot.code,
    type = aov.params$type, effect.size = aov.params$effect.size,
    p.adjust.method = aov.params$p.adjust.method
  )
  if (ext != "Rmd") params[["path"]] <- path
  return(as.character(
    do.call(templates::tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), params))
  ))
}

