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
  df <- do.call(rbind, lapply(dvs, FUN = function(dv) {
  	dat <- as.data.frame(data[,dv])
    colnames(dat) <- c(dv)
    if (!is.null(dv.var)) {
    	dat <- as.data.frame(data[which(data[[dv.var]] == dv), unique(c(dv, group))])
    }
    if (length(group) > 0) dat <- group_by_at(dat, vars(group))
    df <- rstatix::get_summary_stats(dat, type = type)
    if (nrow(df) > 0) return(as.data.frame(df))
  }))
  cnames <- c("n","mean","median","min","max","q1","q3","sd","se","ci","iqr","mad")
  cnames <- unique(c("variable", colnames(df)[!colnames(df) %in% cnames], cnames))
  return(df[,cnames])
}
