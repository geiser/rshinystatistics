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

  as_qq <- function(values, qq) {
    quantiles <- stats::quantile(values)
    sapply(values, FUN = function(x) {
      if (as.numeric(qq) == 2) {
        if (x < quantiles[[3]]) "lower"
        else if (x > quantiles[[3]]) "upper"
        else NA
      } else {
        if (x <= quantiles[[2]]) "low"
        else if (x >= quantiles[[4]]) "high"
        else "medium"
      }
    })
  }

  ## ... perform function

  if (is.null(params)) {
    lvars <- as.list(vars); names(lvars) <- vars
    params <- lapply(lvars, FUN = function(var) {
      if (is.numeric(data[[var]])) {
        return(list(is.numeric = T, var = var, qq = qq))
      } else {
        return(list(is.numeric = F, var = var))
      }
    })
  }

  data <- data[stats::complete.cases(data[,names(params)]),]
  for (i in seq(1,length(params))) {
    param <- params[[i]]
    if (i == 1 && param$is.numeric) {
      data[[param$var]] <- as_qq(data[[param$var]], param$qq)
    } else if (i > 1 && param$is.numeric) {
      dat <- dplyr::group_by_at(data, dplyr::vars(names(params)[1:i-1]))
      for (j in seq(1, nrow(dplyr::group_data(dat)))) {
        idx <- dplyr::group_data(dat)[['.rows']][[j]]
        data[[param$var]][idx] <- as_qq(as.numeric(data[[param$var]][idx]), param$qq)
      }
    }
    data <- data[stats::complete.cases(data[,names(params)]),]
  }

  for (v in names(params)) {
    if (params[[v]]$is.numeric && as.numeric(params[[v]]$qq) == 2) {
      data[[v]] <- factor(data[[v]], levels=c("lower", "upper"), labels=c("lower", "upper"))
    } else if (params[[v]]$is.numeric && as.numeric(params[[v]]$qq) == 3) {
      data[[v]] <- factor(data[[v]], levels=c("low", "medium", "high"), labels=c("low", "medium", "high"))
    }
  }

  return(data)
}

