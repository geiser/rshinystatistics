#' Descriptive Statistics
#'
#' Compute summary of descriptive statistics for numeric variables.
#'
#' @param data a data.frame in which we will perform the descriptive statistics
#' @param dvs numeric columns with the dependent variables
#' @param ivs columns with the independent variables
#' @param type a character string specifying type of summary statistics
#' @param dv.var column with the information to classify observations
#' @return A data.frame containing the results for the descriptive statistics
#' @export
descriptive_statistics <- function(data, dvs, ivs=c(), type = "common", dv.var = NULL) {
  dat <- as.data.frame(data[,c(ivs,dvs)])
  df <- do.call(rbind, lapply(dvs, FUN = function(dv) {
    if (!is.null(dv.var)) dat <- as.data.frame(dat[which(dat[[dv.var]] == dv),])
    if (length(ivs) > 0) dat <- group_by_at(dat, vars(ivs))
    df <- rstatix::get_summary_stats(dat, type = type)
    if (nrow(df) > 0) return(as.data.frame(df))
  }))
  return(df)
}
