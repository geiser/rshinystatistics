#' Ranking Data Based on Quantiles
#'
#' This function ranks numeric data into categories such as high, medium and low based on quantiles.
#' Values lower than q1 (first quantile) are classified as low
#' Values greater than q3 (third quantile) are classified as high
#' Values between q1 and q3 are classified as medium
#'
#' @param data a data.frame to be transformed
#' @param vars numeric columns in which perform the ranking numeric data based on quantiles
#' @return A data.frame containing the ranking data based on quantiles for the columns indicated in the argument vars
#' @export
df2qqs <- function(data, vars) {
  data <- as.data.frame(data)
  for (v in vars) {
    if (is.numeric(data[[v]])) {
      quantiles <- quantile(data[[v]])
      data[[v]] <- sapply(data[[v]], FUN = function(x) {
        if (x <= quantiles[[2]]) "low"
        else if (x >= quantiles[[4]]) "high"
        else "medium"
      })
      data[[v]] <- factor(data[[v]], levels=c("low", "medium", "high"))
    }
  }
  return(data)
}
