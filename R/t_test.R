#' Independent T-Test
#'
#' This function provides a wrapper for rstatic::t_test for multiple dependent variables dvs
#' including their effect sizes
#'
#' @param data a data.frame in which we will perform the t-test
#' @param dvs numeric columns with the dependent variables to be used in the t-test
#' @param iv independent (between) variable in which perform the t-test
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.
#' @param hedges.correction logical value indicating whether apply the Hedges correction
#' @param dv.var column with the information to classify observations
#' @param as.list logical value indicating that the result should be returned as list indicating by separate the t_test and cohens_d effect size
#' @return A data.frame containing the results for the independent t_test or a list with the dataframe in t.test and the t_test with their effect sizes
#' @export
ind_ttest <- function(data, dvs, iv, alternative = 'two.sided', var.equal = FALSE, hedges.correction = FALSE, dv.var = NULL, as.list = FALSE) {
  dat <- data; tt <- list(); ez <- list()
  t.test <- do.call(rbind, lapply(dvs, FUN = function(dv) {
    if (!is.null(dv.var)) dat <- as.data.frame(dat[which(dat[[dv.var]] == dv),])
    sformula <- as.formula(paste0('`',dv,'` ~ `',iv,'`'))
    tt[[dv]] <- rstatix::t_test(dat, sformula, alternative = alternative, var.equal = var.equal, detailed = T)
    ez[[dv]] <- rstatix::cohens_d(dat, sformula, var.equal = var.equal, hedges.correction = hedges.correction)
    return(rstatix::add_significance(merge(tt[[dv]], ez[[dv]])))
  }))
  if (as.list) {
    return(list(t.test = t.test, tt = tt, ez = ez))
  } else return(t.test)
}



