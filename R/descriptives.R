
data.transformation <- function(y, skew = c('posSqrt','negSqrt','posLog','negLog','posInv','negInv')) {
  x <- y
  if (skew == 'posSqrt') {
    x <- y^2
  } else if (skew == 'negSqrt') {
    skewness.code <- paste0('-1*sqrt(max(',initTable,'[[',dvname,']]+1) - ',initTable,'[[',dvname,']])')
  } else if (skew == 'posLog') {
    skewness.code <- paste0('log10(',initTable,'[[',dvname,']])')
  } else if (skew == 'negLog') {
    skewness.code <- paste0('-1*log10(max(',initTable,'[[',dvname,']]+1) - ',initTable,'[[',dvname,']])')
  } else if (skew == 'posInv') {
    skewness.code <- paste0('1/(',initTable,'[[',dvname,']])')
  } else  if (skew == 'negInv') {
    skewness.code <- paste0('-1/(max(',initTable,'[[',dvname,']]+1) - ',initTable,'[[',dvname,']])')
  }
  return(x)
}

#' Descriptive Statistics
#'
#' Compute summary of descriptive statistics for numeric variables.
#'
#' @param data a data.frame in which we will perform the descriptive statistics
#' @param dvs numeric columns with the dependent variables
#' @param ivs columns with the independent variables (split groups)
#' @param type a character string specifying type of summary statistics
#' @param dv.var column with the information to classify observations
#' @param include.global a boolean value to indicate if descriptive statistics for global data is included
#' @param symmetry.test a boolean value to indicate if symmetry test is included
#' @param normality.test a boolean value to indicate if normality test is included
#' @param skewness a list of transformation to achieve normality
#' @return A data.frame containing the results for the descriptive statistics
#' @export
get.descriptives <- function(data, dvs, ivs, type = "common", covar = NULL, dv.var = NULL
                             , include.global = F, symmetry.test = F, normality.test = F
                             , hide.details = F, skewness = c()) {

  tbls <- lapply(dvs, FUN = function(dv) {
    if (is.data.frame(data)) {
      dat <- as.data.frame(data)
      if (!is.null(dv.var)) {
        dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
      }
      divs <- intersect(ivs, colnames(dat))

      for (col in names(skewness))
        dat[[col]] <- dat[[skewness[[col]]]]

      dat <- dat[,c(dv, divs)]
    } else if (is.list(data)) {
      dat <- as.data.frame(data[[dv]])
      divs <- intersect(ivs, colnames(dat))

      for (col in names(skewness))
        dat[[col]] <- dat[[skewness[[col]]]]

      dat <- dat[,c(dv, divs, covar)]
    }

    if (length(divs) > 0)
      dat <- dplyr::group_by_at(dat,  dplyr::vars(divs))

    df <- rstatix::get_summary_stats(dat, type = type)
    if (nrow(df) > 0) {
      for (cname in setdiff(ivs, colnames(df))) {
        df[[cname]] <- NA
      }

      if (include.global)
        df <- plyr::rbind.fill(df, rstatix::get_summary_stats(as.data.frame(dat), dv, type = type))

      return(as.data.frame(df))
    }
  })

  df <- do.call(rbind, tbls)

  if (symmetry.test || normality.test) {
    normality.df <- normality.test.per.groups(data, dvs, ivs, dv.var, include.global, hide.details, skewness = skewness)
    if (!symmetry.test)
      normality.df <- normality.df[,!colnames(normality.df) %in% c("symmetry","skewness","kurtosis")]
    if (!normality.test)
      normality.df <- normality.df[,!colnames(normality.df) %in% c("normality","method","statistic","p","p.signif")]
    df <- merge(df, normality.df, all.x = T, sort =F)
    df <- df[,!colnames(df) %in% 'var']
  }

  cnames <- c("n","mean","median","min","max","q1","q3","sd","se","ci","iqr","mad"
              ,"symmetry","skewness","kurtosis","normality","method","statistic","p","p.signif")
  cnames <- cnames[cnames %in% colnames(df)]
  cnames <- c(colnames(df)[!colnames(df) %in% cnames],cnames)
  return(df[,cnames])
}
