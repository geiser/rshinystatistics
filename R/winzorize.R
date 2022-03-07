winzorize.int <- function (x, minval = NULL, maxval = NULL, probs = c(0.05, 0.95), na.rm = FALSE, type = 7) {
  if (is.null(minval) || is.null(maxval)) {
    xq <- quantile(x = x, probs = probs, na.rm = na.rm, type = type)
    if (is.null(minval))
      minval <- xq[1L]
    if (is.null(maxval))
      maxval <- xq[2L]
  }
  x[x < minval] <- minval
  x[x > maxval] <- maxval
  return(x)
}

#' Winsorization
#'
#' This function performs the replacement of extreme values by less extreme ones
#' in a dataframe for performing parametric tests
#'
#' @param dat a data frame containing the values to be winzorized
#' @param dv column with the dependent variable
#' @param ivs columns with the independent variables
#' @param covar columns with the covariante
#' @param probs numeric vector of probabilities with values in [0,1] as used in quantile
#' @param skewness columns in which there were applied skewness transformation
#' @return A data frame containing the column with dv values replaced
#' @export
winzorize <- function(dat, dv, ivs = NULL, covar = NULL, probs = c(0.05, 0.95), skewness = c()) {
  if (dv %in% colnames(dat)) {
    dat[[dv]] <- winzorize.int(dat[[dv]], probs = probs)
    if (dv %in% skewness)
      dat[[paste0('std.',dv)]] <- winzorize.int(dat[[paste0('std.',dv)]], probs = probs)

    if (length(ivs) > 0) {
      pdat <- dplyr::group_by_at(dat, dplyr::vars(ivs))
      pdat <- dplyr::group_modify(pdat, function(.x,.y) {
        .x[[dv]] <- winzorize.int(.x[[dv]], probs = probs)
        if (dv %in% skewness)
          .x[[paste0('std.',dv)]] <- winzorize.int(.x[[paste0('std.',dv)]], probs = probs)

        if (length(covar) > 0) {
          .x[[covar]] <- winzorize.int(.x[[covar]], probs = probs)
          if (covar %in% skewness)
            .x[[paste0('std.',covar)]] <- winzorize.int(.x[[paste0('std.',covar)]], probs = probs)
        }
        return(.x)
      })
      dat <- as.data.frame(pdat)
    }
  }
  return(dat)
}
