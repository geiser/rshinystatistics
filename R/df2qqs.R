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
    lvars <- as.list(vars); names(lvars) <- vars
    params <- lapply(lvars, FUN = function(var) list(var = var, qq = qq))
  }

  vars <- c()
  for (param in params) {
    if (is.numeric(data[[param$var]])) {
      quantiles <- stats::quantile(data[[param$var]])
      data[[param$var]] <- sapply(data[[param$var]], FUN = function(x) {
        if (as.numeric(param$qq) == 2) {
          if (x < quantiles[[3]]) "lower"
          else if (x > quantiles[[3]]) "upper"
          else NA
        } else {
          if (x <= quantiles[[2]]) "low"
          else if (x >= quantiles[[4]]) "high"
          else "medium"
        }
      })
      vars <- c(vars, param$var)
    }
  }

  data <- data[stats::complete.cases(data[,vars]),]
  for (v in vars) {
    if (as.numeric(params[[v]]$qq) == 2) {
      data[[v]] <- factor(data[[v]], levels=c("lower", "upper"))
    } else {
      data[[v]] <- factor(data[[v]], levels=c("low", "medium", "high"))
    }
  }

  return(data)
}

