#' Symmetry test
#'
#' This function performs a symmetry test in a numeric vector.
#'
#' @param x a numeric vector of data values
#' @return A data frame containing the value of the skewness and kurtosis values
#' @export
symmetry.test <- function(x) {
  skewness <- as.numeric(timeDate::skewness(x, na.rm = T))
  skewness.obs <- 'symmetrical (normal)'
  if (skewness < -2) {
    skewness.obs <- 'negative severe skew'
  } else if (skewness >= -2 && skewness < -1) {
    skewness.obs <- 'negative greater skew'
  } else if (skewness >= -1 && skewness < -0.5) {
    skewness.obs <- 'negative moderate skew'
  } else if (skewness > 0.5 && skewness <= 1) {
    skewness.obs <- 'positive moderate skew'
  } else if (skewness > 1 && skewness <= 2) {
    skewness.obs <- 'positive greater skew'
  } else if (skewness > 2) {
    skewness.obs <- 'positive severe skew'
  }

  kurtosis <- as.numeric(timeDate::kurtosis(x, na.rm = T))
  kurtosis.obs <- 'mesokurtic (normal)'
  if (kurtosis < -3) {
    kurtosis.obs <- 'platykurtic (outliers)'
  } else if (kurtosis > 3) {
    kurtosis.obs <- 'leptokurtic (outliers)'
  }

  return(list(
    skewness = skewness, skewness.obs = skewness.obs,
    kurtosis = kurtosis, kurtosis.obs = kurtosis.obs
  ))
}

#' Normality Test
#'
#' This function performs a normality test in a numeric vector.
#'
#' @param x a numeric vector of data values
#' @return A data frame containing the value of the normality statistic and its corresponding p.value
#' @export
normality.test <- function(x, test = "sw", plimit = 0.05) {
  if (length(unique(x)) < 4) {
    df <- data.frame(
      n = length(x),
      skewness = 0,
      kurtosis = 0,
      symmetry = 'few data',
      statistic = NA,
      method = NA,
      p = 1,
      p.signif = NA,
      normality = 'NO'
    )
    return(df)
  }

  cutpoints <- c(0, 1e-04, 0.001, 0.01, 0.05, 1)
  if (length(x) > 100) {
    cutpoints <- c(0, 1e-05, 1e-04, 0.001, 0.01, 1)
  }

  if (test == "sw") {
    n.test <- shapiro.test(x)
  } else if (test == "da") {
    n.test <- fBasics::dagoTest(x)@test
  }

  normality <- ifelse(n.test$p.value[1] < plimit, 'NO', 'YES')
  if (length(x) > 100) normality <- 'QQ'
  if (length(x) > 200) normality <- '-'

  sd <- symmetry.test(x)

  df <- cbind(rstatix::add_significance(data.frame(
    n = length(x),
    skewness = sd$skewness,
    kurtosis = sd$kurtosis,
    symmetry = ifelse(sd$skewness.obs == 'symmetrical (normal)' && sd$kurtosis.obs == 'mesokurtic (normal)', "YES", "NO"),
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
#' @param dvs a list of numeric columns in which performing the normality test
#' @return A data frame containing the value of the normality statistic and its corresponding p.value
#' @export
normality.test.at <- function(dat, dvs) {
  df <- dplyr::select(dplyr::group_data(dat), -starts_with(".rows"))
  do.call(rbind, lapply(dvs, FUN = function(dv) {
    do.call(rbind, lapply(seq(1, nrow(dplyr::group_data(dat))), FUN = function(i) {
      n.test <- normality.test(dat[[dv]][dplyr::group_data(dat)[[".rows"]][[i]]])
      cbind(variable = dv, df[i,] , n.test)
    }))
  }))
}

#' Normality Test of Residual Model
#'
#' This function performs a normality test in a residual model.
#'
#' @param data a data.frame or list of data containing the variables in which performing the normality test
#' @param dvs a character vector containing the dependent variables
#' @param between a character vector containing the independent variable used as between-subject
#' @param within a character vector containing the independent variable used as within-subject
#' @param covar a character indicating the column of covariate variable used in the normality test
#' @param dv.var column with the information to classify observations based on dependent variables
#' @param skewness a list of transformation to achieve normality
#' @return A data frame containing the normality test
#' @export
normality.test.by.residual <- function(data, dvs, between = c(), within = c(), covar = NULL
                                       , wid = 'row.pos', dv.var = NULL, skewness = c()) {
  result <- do.call(rbind, lapply(dvs, FUN = function(dv) {
    if (is.data.frame(data)) {
      dat <- as.data.frame(data)
      if (!is.null(dv.var))
        dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
    } else if (is.list(data)) {
      dat <- as.data.frame(data[[dv]])
    }

    for (col in names(skewness))
      dat[[col]] <- dat[[skewness[[col]]]]

    within <- within[within %in% colnames(dat)]
    between <- between[between %in% colnames(dat)]

    sformula <- as_formula(dv, between, within, covar, wid)
    if (length(between) == 0 && length(within) == 0 && length(covar) == 0) {
      res <- dat[[dv]]
      names(res) <- dat[[wid]]
    } else if (length(within) > 0) {
      res <- as.data.frame(stats::proj(stats::aov(sformula, data = dat))[[3]])$Residuals
      names(res) <- dat[[wid]]
    } else {
      res <- residuals(lm(sformula, data = dat))
      names(res) <- dat[[wid]]
    }
    df <- normality.test(res)
    if (nrow(df) > 0) return(cbind(var = dv, df))
  }))
  return(result)
}

#' Normality Test per Groups
#'
#' This function performs a normality test per groups.
#'
#' @param data a data.frame containing the variables in which performing the normality test
#' @param dvs a character vector containing the dependent variables
#' @param ivs character vector containing the independent variable used define the groups
#' @param dv.var column with the information to classify observations based on dependent variables
#' @param include.global a boolean value indicating if a summarization is presented of all items
#' @param hide.details a boolean value indicating if details should be presented
#' @param skewness a list of transformation to achieve normality
#' @return A data frame containing the normality test
#' @export
normality.test.per.groups <- function(data, dvs, ivs, dv.var = NULL
                                      , include.global = F, hide.details = F, skewness = c()) {
  non.normal <- do.call(rbind, lapply(dvs, FUN = function(dv) {

    if (is.data.frame(data)) {
      dat <- as.data.frame(data)
      if (!is.null(dv.var))
        dat <- data[which(data[[dv.var]] == dv),]
    } else if (is.list(data)) {
      dat <- as.data.frame(data[[dv]])
    }

    for (col in names(skewness))
      dat[[col]] <- dat[[skewness[[col]]]]

    givs <- unique(ivs[ivs %in% colnames(dat)])
    dat <- dplyr::group_by_at(dat, givs)

    df <- normality.test.at(dat, dv)
    for (cname in setdiff(ivs, colnames(df))) {
      df[[cname]] <- NA
    }

    if (nrow(df) > 0) {
      if (include.global) {
        if (hide.details) df$symmetry <- rep(NA, nrow(df))
        df <- plyr::rbind.fill(df, normality.test.at(as.data.frame(dat), dv))
      }
      return(cbind(var = dv, df))
    }
  }))
  return(non.normal)
}

#' @export
getNonNormal <- function(x, x.name = paste0('', seq(1, length(x))), step = 1,
                         plimit = NULL, max.step = step+6) {
  if (length(unique(x)) < 4) return(c())
  names(x) <- x.name

  if (is.null(plimit) && length(x) > 100)
    plimit <- 0.01
  else if(is.null(plimit))
    plimit <- 0.05

  toReturn <- c()
  max.length <- Inf
  for (pstep in (max.step:step)) {
    xtemp <- x
    non.normal <- c()
    res <- tryCatch(normality.test(xtemp), error = function(e) NULL)
    while(!is.null(res) && (length(unique(xtemp)) > 3) && res$p < plimit) {
      y <- sort(xtemp)
      xnorm <- qqnorm(y, plot.it = F)
      qqline <- getQQline(y)
      y.diff <- (xnorm$y-((qqline$slope*xnorm$x)+as.numeric(qqline$intercept)))^2
      y.diff <- sort(y.diff, decreasing = T)[1:pstep]
      xtemp <- xtemp[!names(xtemp) %in% names(y.diff)]
      non.normal <- c(non.normal, names(y.diff))
      res <- tryCatch(normality.test(xtemp), error = function(e) NULL)
    }
    if (length(non.normal) > 0 && length(non.normal) < max.length) {
      toReturn <- non.normal
      max.length <- length(toReturn)
    }
  }

  return(toReturn)
}
