#' Identifying Outliers Using BoxPlot Methods
#'
#' This function provides a wrapper for rstatic::identify_outliers for multiple dependent variables dvs
#'
#' @param data a data.frame in which we will perform the identification of outliers
#' @param dvs numeric columns with the dependent variables
#' @param ivs columns to be used as independent variable in which perform the identification of outliers
#' @param covar column with the covariant variable in which perform the outlier identification
#' @return A data.frame containing the results rstatix::identify_outliers with column var in which is indicating each dependent variable
#' @export
getOutliers <- function (data, dvs, ivs = c(), covar = c(), is.extreme = T) {
  dat <- data
  if (length(ivs) > 0)
    dat <- dplyr::group_by_at(data, dplyr::vars(ivs))

  df.out <- do.call(rbind, lapply(dvs, FUN = function(dv) {
    outliers <- rstatix::identify_outliers(dat, variable = dv, coef = 1.25)
    if (nrow(outliers) > 0 && length(covar) > 0) {
      covarout <- rstatix::identify_outliers(dat, variable = covar, coef = 1.25)
      if (!is.null(covarout) && nrow(covarout) > 0) {
        cnames <- intersect(colnames(covarout), colnames(outliers))
        outliers <- plyr::rbind.fill(outliers[,cnames], covarout[,cnames])
      }
    }
    if (nrow(outliers) > 0) return(cbind(var = dv, outliers))
  }))

  if (is.extreme) return (df.out[df.out[["is.extreme"]] == is.extreme,])
  return(df.out)
}
