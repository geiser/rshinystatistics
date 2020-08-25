#' Identifying Outliers Using BoxPlot Methods
#'
#' This function provides a wrapper for rstatic::identify_outliers for multiple dependent variables dvs
#'
#' @param data a data.frame in which we will perform the identification of outliers
#' @param dvs numeric columns with the dependent variables
#' @param ivs columns to be used as independent variable in which perform the identification of outliers
#' @return A data.frame containing the results rstatix::identify_outliers with column var in which is indicating each dependent variable
#' @export
getOutliers <- function (data, dvs, ivs = c()) {
  dat <- data
  if (length(ivs) > 0) dat <- dplyr::group_by_at(data, dplyr::vars(ivs))
  do.call(rbind, lapply(dvs, FUN = function(dv) {
    outliers <-  rstatix::identify_outliers(dat, variable = dv)
    if (nrow(outliers) > 0) return(cbind(var = dv, outliers))
  }))
}
