#' Ranking Data Based on Quantiles
#'
#' This function ranks numeric data into categories such as high, medium and low based on quantiles.
#' Values lower than q1 (first quantile) are classified as low
#' Values greater than q3 (third quantile) are classified as high
#' Values between q1 and q3 are classified as medium
#'
#' @param data a data.frame to be transformed
#' @param vars numeric columns in which perform the ranking numeric data based on quantiles
#' @param qq numeric value {2, 3} to define if the transformation will be carried out in {lower/upper, low/medium/high}
#' @param  params a list with the pairs list(var = var, qq = qq) parameters to be employed for the transformation
#' @return A data.frame containing the ranking data based on quantiles for the columns indicated in the vars argument
#' @export
df2qqs <- function(data, vars = c(), qq = 3, params = NULL) {

  if (is.null(params)) {
    params <- lapply(vars, FUN = function(var) list(var = var, qq = qq))
  }

  vars <- c()
  for (p in params) {
    if (is.numeric(data[[p$var]])) {
      quantiles <- quantile(data[[p$var]])
      data[[p$var]] <- sapply(data[[p$var]], FUN = function(x) {
        if (p$qq == 2) {
          if (x < quantiles[[3]]) "lower"
          else if (x > quantiles[[3]]) "upper"
          else NA
        } else {
          if (x <= quantiles[[2]]) "low"
          else if (x >= quantiles[[4]]) "high"
          else "medium"
        }
      })
      vars <- c(vars, p$var)
    }
  }

  data <- data[stats::complete.cases(data[,vars]),]
  for (v in vars) {
    if (params[[v]]$qq == 2) {
      data[[v]] <- factor(data[[v]], levels=c("lower", "upper"))
    } else {
      data[[v]] <- factor(data[[v]], levels=c("low", "medium", "high"))
    }
  }
  return(data)
}
