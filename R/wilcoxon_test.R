#' Wilcoxon Test
#'
#' This function provides a wrapper for rstatic::wilcox_test for multiple dependent variables dvs
#' including their effect sizes
#'
#' @param data a data.frame in which we will perform the wilcoxon-test
#' @param dvs numeric columns with the dependent variables to be used in the wilcoxon-test
#' @param iv independent (between) variable in which perform the wilcoxon-test
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param dv.var column with the information to classify observations
#' @param as.list logical value indicating that the result should be returned as list indicating by separate the wilcoxon_test and effect size
#' @return A data.frame containing the results for the independent wilcoxon_test or a list with the dataframe in wilcoxon.test and the wilcoxon_test with their effect sizes
#' @export
wilcoxon_test <- function(data, dvs, iv, alternative = 'two.sided', dv.var = NULL, as.list = FALSE) {
  dat <- as.data.frame(data)
  ldvs <- as.list(dvs); names(ldvs) <- dvs
  wt <- lapply(ldvs, FUN = function(dv) {
    if (!is.null(dv.var))
      dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
    sformula <- as.formula(paste0('`',dv,'` ~ `',iv,'`'))
    return(rstatix::wilcox_test(dat, sformula, alternative = alternative, detailed = T))
  })
  ez <- lapply(ldvs, FUN = function(dv) {
    if (!is.null(dv.var))
      dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
    sformula <- as.formula(paste0('`',dv,'` ~ `',iv,'`'))
    return(rstatix::wilcox_effsize(dat, sformula, alternative = alternative))
  })
  wilcoxon.test <- do.call(rbind, lapply(dvs, FUN = function(dv) {
    return(rstatix::add_significance(merge(wt[[dv]], ez[[dv]])))
  }))
  if (as.list) {
    return(list(wilcoxon.test = wilcoxon.test, wt = wt, ez = ez))
  } else return(wilcoxon.test)
}



