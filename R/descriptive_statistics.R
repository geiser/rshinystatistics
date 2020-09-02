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
  tbls <- lapply(dvs, FUN = function(dv) {
    if (is.data.frame(data)) {
      dat <- as.data.frame(data)
      if (!is.null(dv.var)) {
        dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
      }
      divs <- intersect(ivs, colnames(dat))
      dat <- dat[,c(dv, divs)]
    } else if (is.list(data)) {
      dat <- dat <- as.data.frame(data[[dv]])
      divs <- intersect(ivs, colnames(dat))
      dat <- dat[,c(dv, divs)]
    }
    if (length(divs) > 0)
      dat <- dplyr::group_by_at(dat,  dplyr::vars(divs))


    df <- rstatix::get_summary_stats(dat, type = type)
    if (nrow(df) > 0) {
      for (cname in setdiff(ivs, colnames(df))) {
        df[[cname]] <- NA
      }
      return(as.data.frame(df))
    }
  })

  df <- do.call(rbind, tbls)

  cnames <- c("n","mean","median","min","max","q1","q3","sd","se","ci","iqr","mad")
  cnames <- cnames[cnames %in% colnames(df)]
  cnames <- c(colnames(df)[!colnames(df) %in% cnames],cnames)
  return(df[,cnames])
}
