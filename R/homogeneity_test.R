#' Homogeneity Test of Variance 
#'
#' This function performs a homogeneity test of variance.
#'
#' @param data a data.frame containing the variables in which performing the homogenity test
#' @param dvs a character vector containing the dependent variables
#' @param between a character vector containing the independent variable used between-subject
#' @param within a character vector containing the independent variable used within-subject
#' @param dv.var column with the information to classify observations based on dependent variables
#' @return A data frame containing the homogeneity test (levene's test)
#' @export
homogeneity_test <- function(data, dvs, between = c(), within = c(), dv.var = NULL) {
  dat <- data.frame(data)
  levene.test <- do.call(rbind, lapply(dvs, FUN = function(dv) {
    if (!is.null(dv.var)) dat <- data[which(data[[dv.var]] == dv),]
    sformula <- paste(paste0('`', dv, '`'), "~", paste0(paste0('`', between, '`'), collapse = "*"))
    df <- rstatix::levene_test(dat, as.formula(sformula))
    if (nrow(df) > 0) return(cbind(var = dv,  rstatix::add_significance(df)))
  }))
  return(levene.test)
}
