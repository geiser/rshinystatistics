
hypothesisAsFile <- function(ext, test, backup, dvs = 'dvs', between = 'between', covar = 'covar', path = getwd(), lang='en') {

  wid <- backup$variables$wid
  rdvs <- unique(unlist(backup$variables[c(dvs)], use.names = F))
  rbetween <- unique(unlist(backup$variables[c(between)], use.names = F))
  rcovar <- unique(unlist(backup$variables[c(covar)], use.names = F))

  code.skewness <- paste0(lapply(rdvs, FUN = function(dv) {
    line.code <- skewness.as.code(backup$skewness[[dv]], paste0('"',dv,'"'), paste0('dat[["',dv,'"]]'), paste0('rdat[["',dv,'"]]'))
    if (is.null(line.code)) return("")
    if (length(backup$skewness) > 0 && length(rcovar) > 0 && length(backup$skewness[[rcovar]]) > 0) {
      line.code <- paste0(
        line.code, '\n',
        skewness.as.code(backup$skewness[[rcovar]], paste0('"',rcovar,'"')
                      , initTable=paste0('dat[["',dv,'"]]'), dataTable=paste0('rdat[["',dv,'"]]'))
      )
    }

    line.code <- paste0(c(
      paste0('density.plot.by.residual(rdat[["',dv,'"]],"',dv,'",between',ifelse(length(rcovar) > 0 && length(backup$skewness[[rcovar]]) > 0,',c(),covar',''),')'),
      line.code,
      paste0('density.plot.by.residual(rdat[["',dv,'"]],"',dv,'",between',ifelse(length(rcovar) > 0 && length(backup$skewness[[rcovar]]) > 0,',c(),covar',''),')')),
      collapse = "\n")

    if (ext == 'Rmd') {
      line.code <- paste0("\n```{r}\n",line.code,"\n```\n", "\n")
    }
    if (lang=='pt')
      return(paste0('\n Aplicando transformação in "',dv,'" para reduzir distorsão\n',line.code))
    else
      return(paste0('\n Applying transformation in "',dv,'" to reduce skewness\n',line.code))
  }), collapse = "\n")

  test.params <- backup[[paste0(test,'Params')]][['hypothesis']]
  tfile <- system.file("templates/code", paste0("nonParamHypothesisTest",ifelse(lang!='en',paste0('-',lang),''),".",ext), package="rshinystatistics")
  if (test %in% c('ancova','anova','ttest')) {
    tfile <- system.file("templates/code", paste0("paramHypothesisTest",ifelse(lang!='en',paste0('-',lang),''),".",ext), package="rshinystatistics")
  }

  code.outliers <- ''
  if (backup$outlier.method == 'remove') {
    code.outliers <- list.as.code(backup$outliers)
  } else if (backup$outlier.method == 'winsorize') {
    code.outliers <- do.call(paste0, lapply(rdvs, FUN = function(dv) {
      paste0('rdat[["',dv,'"]] <- winzorize(rdat[["',dv,'"]],"',dv,'", c(',paste0(paste0('"',rbetween,'"'),collapse=','),')',ifelse(length(rcovar) > 0 && length(backup$skewness[[rcovar]]) > 0,',covar',''),')\n')
    }))
    if (ext == "Rmd") {
      code.outliers <- paste0(c("```{r}", code.outliers, "```"), collapse = '\n')
    }
  }

  linearity.code <- ''
  if (test %in% c('ancova')) {
    linearity.code <- paste0(lapply(rdvs, FUN = function(dv) {
      line.code <- linearity.as.code(backup, paste0('sdat[["',dv,'"]]'), paste0('"',dv,'"'), 'covar', 'between', ext)
      return(line.code)
    }), collapse = "\n")
  }

  code.pwc <- ""; code.pwc.tbl <- ""
  code.hypothesis <- ""; code.hypothesis.tbl <- ""
  if ('wilcoxon' == test) {
    title.test = 'Wilcoxon Signed-Rank'
    code.plots <- wilcoxon.as.code.plots(backup, 'sdat', rdvs, rbetween, ext)

    code.hypothesis <- paste0('res <- wilcoxon.test(sdat, dvs, between, "', test.params$alternative ,'", as.list=T)',
                              '\n', '(wdf <- res$wilcoxon.test)')
    code.hypothesis.tbl <- 'kable(wdf[,c(".y.","group1","group2","n1","n2","statistic","estimate","conf.low","conf.high","effsize","magnitude","p","p.signif")], digits=3)'

    code.pwc <- ''; code.pwc.tbl <- ''; hypothesis.pwc.text <- ''
    hypothesis.text <- wilcoxon.as.text(backup$wilcoxon, backup$ds, rbetween, lang=lang)
  } else if ('ttest' == test) {
    title.test = 'Independent t-Test'
    code.plots <- ind.ttest.as.code.plots(backup, 'sdat', rdvs, rbetween, ext)

    code.hypothesis <- paste0('res <- ind.ttest(sdat, dvs, between, "', test.params$alternative ,'",',test.params$var.equal,',',test.params$hedges.correction,', as.list=T)',
                              '\n', '(tdf <- res$t.test)')
    code.hypothesis.tbl <- 'kable(tdf[,c(".y.","group1","group2","n1","n2","statistic","estimate","conf.low","conf.high","effsize","magnitude","p","p.signif")], digits=3)'

    code.pwc <- ''; code.pwc.tbl <- ''; hypothesis.pwc.text <- ''

    code.emms <- paste0('(df <- get.descriptives(sdat, dvs, between, "common"))')
    code.emms.tbl <- 'kable(df[,c("variable",colnames(df)[!colnames(df) %in% c("n","mean","median","min","max","q1","q3","sd","se","ci","iqr","mad")],"n","mean","median","sd","se","ci")], digits = 3)'

    hypothesis.text <- ttest.as.text(backup$ttest, backup$ds, rbetween, test.params$var.equal, test.params$hedges.correction, lang = lang)
  } else if ('kruskal' == test) {
    title.test = 'Kruskal–Wallis test'
    code.plots <- kruskal.as.code.plots(backup, 'sdat', rdvs, rbetween, ext)

    code.hypothesis <- paste0(c("kruskal <- kruskal.test(sdat, dvs, between)",
                                "(kdf <- get.kruskal.table(kruskal))"), collapse = '\n')
    code.hypothesis.tbl <- 'kable(kdf[,c("var","n","df","statistic","effsize","magnitude","p","p.signif")], digits = 3)'

    code.pwc <- paste0('pwc <- kruskal.pwc(sdat, dvs, between, pwc.method = "', test.params$pwc.method ,'", p.adjust.method = "', test.params$p.adjust.method ,'")',
                       '\n','(pdf <- get.kruskal.pwc.table(pwc, only.sig = F))')
    code.pwc.tbl <- 'kable(pdf[,c("var","group1","group2","n1","n2","estimate","statistic","p","p.adj","p.adj.signif")], digits = 3)'

    hypothesis.text <- kruskal.as.text(backup$kruskal, backup$dataTable, rbetween, lang=lang)
    hypothesis.pwc.text <- wilcoxon.pwc.as.text(backup$pwc, backup$ds, rbetween, p.adjust.method = test.params$p.adjust.method, lang=lang)
  } else if ('srh' == test) {
    title.test = 'Scheirer-Ray-Hare test'
    code.plots <- srh.as.code.plots(backup, 'sdat', rdvs, rbetween, ext)

    code.hypothesis <- paste0(c("srh <- scheirer.test(sdat, dvs, between)",
                                "(sdf <- get.scheirer.table(srh))"), collapse = '\n')
    code.hypothesis.tbl <- 'kable(sdf[,c("var","Effect","Df","Sum Sq","H","p.value","p.value.signif")], digits = 3)'

    code.pwc <- paste0('pwc <- scheirer.pwc(sdat, dvs, between, pwc.method = "', test.params$pwc.method ,'", p.adjust.method = "', test.params$p.adjust.method ,'")',
                       '\n','(pdf <- get.scheirer.pwc.table(pwc, only.sig = F))')
    code.pwc.tbl <- 'kable(pdf[,c("var",c(between),"group1","group2","estimate","statistic","p", "p.adj","p.adj.signif")], digits = 3)'

    hypothesis.text <- srh.as.text(backup[[test]], backup$dataTable, rbetween, lang=lang)
    hypothesis.pwc.text <- wilcoxon.pwc.as.text(backup$pwc, backup$ds, rbetween, p.adjust.method = test.params$p.adjust.method, lang=lang)
  } else if ('ancova' == test) {
    title.test = 'ANCOVA test'
    code.plots <- ancova.as.code.plots(backup, 'sdat', rdvs, rbetween, rcovar, ext)

    code.hypothesis <- paste0('aov <- ancova.test(sdat, dvs, between, covar, ', test.params$type ,', "', test.params$effect.size ,'")',
                              '\n','(adf <- get.ancova.table(aov))')
    code.hypothesis.tbl <- paste0('kable(adf[,c("var","Effect","DFn","DFd","SSn","SSd","F","p","',test.params$effect.size,'","p.signif")], digits=3)')

    code.pwc <- paste0('pwc <- ancova.pwc(sdat, dvs, between, covar, p.adjust.method = "', test.params$p.adjust.method ,'")',
                       '\n','(pdf <- get.ancova.pwc.table(pwc, only.sig = F))')
    code.pwc.tbl <- 'kable(pdf[,c("var",between,"group1","group2","estimate","conf.low","conf.high","se","statistic","p","p.adj","p.adj.signif")], digits = 3)'

    code.emms <- paste0('(emms <- get.ancova.emmeans.with.ds(pwc, sdat, dvs, between, "common", covar = covar))')
    code.emms.tbl <- 'kable(emms, digits = 3)'

    hypothesis.text <- ancova.as.text(backup[[test]], backup$dataTable, rbetween, rcovar, test.params$effect.size, lang=lang)
    hypothesis.pwc.text <- aov.pwc.as.text(test, backup$pwc, backup$ds, rbetween, p.adjust.method = test.params$p.adjust.method, lang=lang)
  } else if ('anova' == test) {
    title.test = 'ANOVA test'
    code.plots <- anova.as.code.plots(backup, 'sdat', rdvs, rbetween, ext)

    code.hypothesis <- paste0('aov <- anova.test(sdat, dvs, between, type=', test.params$type ,', effect.size="', test.params$effect.size ,'")',
                              '\n','(adf <- get.anova.table(aov))')
    code.hypothesis.tbl <- paste0('kable(adf[,c("var","Effect","DFn","DFd","SSn","SSd","F","p","',test.params$effect.size,'","p.signif")], digits=3)')

    code.pwc <- paste0('pwc <- anova.pwc(sdat, dvs, between, p.adjust.method = "', test.params$p.adjust.method ,'")',
                       '\n','(pdf <- get.anova.pwc.table(pwc, only.sig = F))')
    code.pwc.tbl <- 'kable(pdf[,c("var",between,"group1","group2","estimate","conf.low","conf.high","se","statistic","p","p.adj","p.adj.signif")], digits = 3)'

    code.emms <- paste0('(emms <- get.anova.emmeans.with.ds(pwc, sdat, dvs, between, "common"))')
    code.emms.tbl <- 'kable(emms[,c("var",between,"n","emmean","mean","conf.low","conf.high","sd","sd.emms","se.emms")], digits = 3)'

    hypothesis.text <- anova.as.text(backup[[test]], backup$dataTable, rbetween, test.params$effect.size, lang=lang)
    hypothesis.pwc.text <- aov.pwc.as.text(test, backup$pwc, backup$ds, rbetween, p.adjust.method = test.params$p.adjust.method, lang=lang)
  }

  if (ext == "Rmd") {
    code.hypothesis <- paste0(c("```{r, include=FALSE}", code.hypothesis, "```"), collapse = '\n')
    code.hypothesis.tbl <- paste0(c("```{r, echo=FALSE, purl=FALSE}", code.hypothesis.tbl, "```"), collapse = '\n')

    if ('wilcoxon' != test) {
      code.pwc <- paste0(c("### Pairwise comparison","","```{r, include=FALSE}", code.pwc, "```"), collapse = '\n')
      code.pwc.tbl <- paste0(c("```{r, echo=FALSE, purl=FALSE}", code.pwc.tbl, "```"), collapse = '\n')
    }
  }

  params <- list(
    rshinystatistics.version = as.character(packageVersion("rshinystatistics")),
    test = test, title.test = title.test, author = backup$author, email = backup$email,
    wid = wid, dvs = rdvs, between = rbetween, covar = NULL,
    code.outliers =  code.outliers, code.skewness = code.skewness,
    code.hypothesis = code.hypothesis, code.hypothesis.tbl = code.hypothesis.tbl,
    code.pwc = code.pwc, code.pwc.tbl = code.pwc.tbl, code.plots = code.plots
  )

  if (test %in% c('ancova','anova','ttest')) {
    if (test == 'ancova') params[['covar']] <- rcovar
    params[['code.non.normal']] <- list.as.code(backup$toRemoveForNormality)

    theader <- "### Descriptive Statistic of Estimated Marginal Means"
    if (test == 'ttest') theader <- "### Descriptive Statistic"
    params[['code.emms']] <- paste0(c(theader,"","```{r, include=FALSE}", code.emms, "```"), collapse = '\n')
    params[['code.emms.tbl']] <- paste0(c("```{r, echo=FALSE, purl=FALSE}", code.emms.tbl, "```"), collapse = '\n')
  }

  if (ext != "Rmd") {
    params[["path"]] <- path
  } else {
    params[["hypothesis.text"]] <- hypothesis.text
    params[["hypothesis.pwc.text"]] <- hypothesis.pwc.text
  }

  return(as.character(
    do.call(templates::tmpl, c(list(".t" = paste(readLines(tfile), collapse="\n")), params))
  ))
}

