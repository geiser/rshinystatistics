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
  if (length(x) > 100) {
    plimit <- 0.001
    cutpoints <- c(0, 1e-06, 1e-05, 1e-04, 0.001, 1)
  }
  if (length(x) > 50) n.test <- fBasics::dagoTest(x)@test

  normality <- ifelse(n.test$p.value[1] < plimit, 'NO', 'YES')
  if (length(x) > 100) normality <- 'QQ'
  if (length(x) > 200) normality <- '-'

  df <- cbind(add_significance(data.frame(
    n = length(x),
    statistic = n.test$statistic[1],
    method = strsplit(n.test$method, ' ')[[1]][1],
    p = n.test$p.value[1]
  ), p.col = "p", cutpoints = cutpoints), normality = normality)
  rownames(df) <- rep('', nrow(df))
  return(df)
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

#' Normality Test of Residual Model
#'
#' This function performs a normality test in a residual model.
#'
#' @param data a data.frame containing the variables in which performing the normality test
#' @param dvs a character vector containing the dependent variables
#' @param between a character vector containing the independent variable used between-subject
#' @param within a character vector containing the independent variable used within-subject
#' @param covar a character indicating the column of covariate variable used in the normality test
#' @param dv.var column with the information to classify observations based on dependent variables
#' @return A data frame containing the normality test
#' @export
normality_test_by_res <- function(data, dvs, between, within = c(), covar = NULL, dv.var = NULL) {
  dat <- as.data.frame(data) 
  non.normal <- do.call(rbind, lapply(dvs, FUN = function(dv) {
    if (!is.null(dv.var)) dat <- data[which(data[[dv.var]] == dv),]
    sformula <- paste0(paste0('`',between,'`'), collapse = '*')
    if (!is.null(covar)) sformula <- paste0('`', covar, '` + ', sformula)
    sformula <- as.formula(paste0('`', dv, '` ~ ', sformula))
    mdl <- lm(sformula, data = dat)
    df <- normality_test(residuals(mdl))
    if (nrow(df) > 0) return(cbind(var = dv, df))
  }))
  return(non.normal)
}

#' Normality Test per Groups
#'
#' This function performs a normality test per groups.
#'
#' @param data a data.frame containing the variables in which performing the normality test
#' @param dvs a character vector containing the dependent variables
#' @param vars character vector containing the independent variable used define the groups
#' @param dv.var column with the information to classify observations based on dependent variables
#' @return A data frame containing the normality test
#' @export
normality_test_per_group <- function(data, dvs, vars, dv.var = NULL) {
  dat <- as.data.frame(data)
  non.normal <- do.call(rbind, lapply(dvs, FUN = function(dv) {
    if (!is.null(dv.var)) dat <- data[which(data[[dv.var]] == dv),]
    dat <- dplyr::group_by_at(dat, vars)
    df <- normality_test_at(dat, dv)
    if (nrow(df) > 0) return(cbind(var = dv, df))
  }))
  return(non.normal)
}

