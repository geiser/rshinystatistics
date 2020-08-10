#' Normality Test
#'
#' This function performs a normality test in a numeric vector.
#'
#' @param x a numeric vector of data values
#' @return A data frame containing the value of the normality statistic and its corresponding p.value
#' @export
normality_test <- function(x) {
  plimit <- 0.05
  n.test <- shapiro.test(x)
  cutpoints <- c(0, 1e-04, 0.001, 0.01, 0.05, 1)

  if (length(x) > 30) {
    plimit <- 0.01
    cutpoints <- c(0, 1e-05, 1e-04, 0.001, 0.01, 1)
  }
  if (length(x) > 50) n.test <- dagoTest(x)@test

  normality <- ifelse(n.test$p.value[1] < plimit, 'NO', 'YES')
  if (length(x) > 100) normality <- 'QQ'
  if (length(x) > 200) normality <- '-'

  return(cbind(add_significance(data.frame(
    n = length(x),
    statistic = n.test$statistic[1],
    method = strsplit(n.test$method, ' ')[[1]][1],
    p = n.test$p.value[1]
  ), p.col = "p", cutpoints = cutpoints), normality = normality))
}

#' Normality Test
#'
#' This function performs a normality test in diferents columns of vector.
#'
#' @param dat a tibble data.frame containing the variables in which performing the normality test
#' @param vars a list of numeric columns in which performing the normality test
#' @return A data frame containing the value of the normality statistic and its corresponding p.value
#' @export
normality_test_at <- function(dat, vars) {
  df <- select(group_data(dat), -starts_with(".rows"))
  do.call(rbind, lapply(vars, FUN = function(v) {
    do.call(rbind, lapply(seq(1, nrow(group_data(dat))), FUN = function(i) {
      n.test <- normality_test(dat[[v]][group_data(dat)[[".rows"]][[i]]])
      cbind(variable = v, df[i,] , n.test)
    }))
  }))
}
