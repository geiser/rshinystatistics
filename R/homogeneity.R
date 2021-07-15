#' Homogeneity test for ANCOVA and ANOVA test
#'
#' This function performs a homogeneity test of variance.
#'
#' @param data a data.frame containing the variables in which performing the homogenity test
#' @param dvs a character vector containing the dependent variables
#' @param between a character vector containing the independent variable used between-subject
#' @param within a character vector containing the independent variable used within-subject
#' @param covar column with the variable to be used as covariance
#' @param dv.var column with the information to classify observations based on dependent variables
#' @return A data frame containing the homogeneity test (levene's test of variances and Anova slopes in ancova)
#' @export
homogeneity.test <- function(data, dvs, between = c(), within = c(), covar = NULL, dv.var = NULL) {
  do.call(rbind, lapply(dvs, FUN = function(dv) {
    if (is.data.frame(data)) {
      dat <- as.data.frame(data)
      if (!is.null(dv.var))
        dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
    } else if (is.list(data)) {
      dat <- as.data.frame(data[[dv]])
    }

    sformula <- as_formula(dv, between, within, as.character = T)
    if (!is.null(covar) && length(covar) > 0) {
      dat[[".res"]] <- stats::residuals(stats::lm(as_formula(dv, between, within, covar), data = dat))
      sformula <- as_formula(".res", between, within, as.character = T)
    }

    df.test <- rstatix::levene_test(dat, stats::as.formula(sformula))
    if (!is.null(covar) && length(covar) > 0) {
      colnames(df.test) <- c('DFn.df1', 'DFd.df2', 'statistic', 'p')
    }
    df.test <- cbind(method = "Levene's test", formula = sformula, n = nrow(dat), df.test)

    if (!is.null(covar) && length(covar) > 0) {
      dat[[".group"]] <- substring(do.call(paste0, lapply(c(between,within), FUN = function(cname) {
        paste0(':',dat[[cname]])
      })), 2)
      aov <- rstatix::anova_test(as.data.frame(dat), stats::as.formula(paste0(dv, " ~ .group*",covar)))
      idx <- which(aov[["Effect"]] == paste0(".group:", covar))
      df.test <- rbind(df.test, data.frame(
        method = "Anova's slopes",
        formula = sformula,
        n = nrow(dat),
        statistic = aov[["F"]][idx],
        DFn.df1 = aov[["DFn"]][idx],
        DFd.df2 = aov[["DFd"]][idx],
        p = aov[["p"]][idx]))
    }

    # ... add significance
    plimit <- 0.05
    cutpoints <- c(0, 1e-04, 0.001, 0.01, 0.05, 1)
    if (length(dat[[dv]]) > 100) {
      plimit <- 0.01
      cutpoints <- c(0, 1e-05, 1e-04, 0.001, 0.01, 1)
    }

    rstatix::add_significance(cbind(
      var = dv, df.test
    ), p.col = "p", cutpoints = cutpoints)
  }))
}
