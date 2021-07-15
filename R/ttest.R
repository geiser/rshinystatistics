#' Independent T-Test
#'
#' This function provides a wrapper for rstatic::t_test for the dependent variables `dvs`
#' including their effect sizes
#'
#' @param data a data.frame in which we will perform the t-test
#' @param dvs numeric columns with the dependent variables to be used in the indepedent t-test
#' @param iv independent (between) variable in which perform the t-test
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.
#' @param hedges.correction logical value indicating whether apply the Hedges correction
#' @param dv.var column with the information to classify observations
#' @param as.list logical value indicating that the result should be returned as list indicating by separate the t_test and cohens_d effect size
#' @return A data.frame containing the results for the independent t_test or a list with the dataframe in t.test and the t_test with their effect sizes
#' @export
ind.ttest <- function(data, dvs, iv, alternative = 'two.sided', var.equal = FALSE, hedges.correction = TRUE, dv.var = NULL, as.list = FALSE) {
  ldvs <- as.list(dvs); names(ldvs) <- dvs

  tt <- lapply(ldvs, FUN = function(dv) {
    if (is.data.frame(data)) {
      dat <- as.data.frame(data)
      if (!is.null(dv.var))
        dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
    } else if (is.list(data)) {
      dat <- as.data.frame(data[[dv]])
    }
    sformula <- as.formula(paste0('`',dv,'` ~ `',iv,'`'))
    return(rstatix::t_test(dat, sformula, alternative = alternative, var.equal = var.equal, detailed = T))
  })

  ez <- lapply(ldvs, FUN = function(dv) {
    if (is.data.frame(data)) {
      dat <- as.data.frame(data)
      if (!is.null(dv.var))
        dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
    } else if (is.list(data)) {
      dat <- as.data.frame(data[[dv]])
    }
    sformula <- as.formula(paste0('`',dv,'` ~ `',iv,'`'))
    return(rstatix::cohens_d(dat, sformula, var.equal = var.equal, hedges.correction = hedges.correction))
  })
  t.test <- do.call(rbind, lapply(dvs, FUN = function(dv) {
    return(rstatix::add_significance(merge(tt[[dv]], ez[[dv]])))
  }))
  if (as.list) {
    return(list(t.test = t.test, tt = tt, ez = ez))
  } else return(t.test)
}

#' Paired T-Test
#'
#' This function provides a wrapper for rstatic::t_test using various dependent variables `dvs`
#' including their effect sizes
#'
#' @param data a data.frame in which we will perform the t-test
#' @param dvs numeric columns with the dependent variables to be used in the paired t-test
#' @param iv independent (between) variable in which perform the t-test
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.
#' @param hedges.correction logical value indicating whether apply the Hedges correction
#' @param dv.var column with the information to classify observations
#' @param as.list logical value indicating that the result should be returned as list indicating by separate the t_test and cohens_d effect size
#' @return A data.frame containing the results for the independent t_test or a list with the dataframe in t.test and the t_test with their effect sizes
paired.ttest <- function(data, dvs, iv, alternative = 'two.sided', var.equal = FALSE, hedges.correction = TRUE, dv.var = NULL, as.list = FALSE) {
  ldvs <- as.list(dvs); names(ldvs) <- dvs

  tt <- lapply(ldvs, FUN = function(dv) {
    if (is.data.frame(data)) {
      dat <- as.data.frame(data)
      if (!is.null(dv.var))
        dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
    } else if (is.list(data)) {
      dat <- as.data.frame(data[[dv]])
    }

    int.iv <- intersect(iv, colnames(dat))
    int.iv <- int.iv[1]

    sformula <- as.formula(paste0('`',dv,'` ~ `',int.iv,'`'))
    return(rstatix::t_test(dat, sformula, alternative = alternative, var.equal = var.equal, paired = T, detailed = T))
  })

  ez <- lapply(ldvs, FUN = function(dv) {
    if (is.data.frame(data)) {
      dat <- as.data.frame(data)
      if (!is.null(dv.var))
        dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
    } else if (is.list(data)) {
      dat <- as.data.frame(data[[dv]])
    }

    int.iv <- intersect(iv, colnames(dat))
    int.iv <- int.iv[1]

    sformula <- as.formula(paste0('`',dv,'` ~ `',int.iv,'`'))
    return(rstatix::cohens_d(dat, sformula, var.equal = var.equal, paired = T,  hedges.correction = hedges.correction))
  })
  t.test <- do.call(rbind, lapply(dvs, FUN = function(dv) {
    return(rstatix::add_significance(merge(tt[[dv]], ez[[dv]])))
  }))
  if (as.list) {
    return(list(t.test = t.test, tt = tt, ez = ez))
  } else return(t.test)
}
