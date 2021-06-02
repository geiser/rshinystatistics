#' Non Parametric Wilcoxon Plot
#'
#' @export
ggPlotWilcoxon <- function(data, x, y, wt, addParam = c(), font.label.size = 14) {
  stat.test <- rstatix::add_xy_position(rstatix::add_significance(wt), x=x)
  bxp <- ggpubr::ggboxplot(
    data, x=x, y=y, color=x, width=0.5, add=addParam, palette="jco"
  )
  bxp <- bxp + ggpubr::stat_pvalue_manual(stat.test, hide.ns=T, tip.length = 0)
  bxp <- bxp + ggplot2::labs(subtitle = rstatix::get_test_label(stat.test, detailed=T))
  bxp <- bxp + ggplot2::theme(text = ggplot2::element_text(size=font.label.size))
  return(bxp)
}


#' Non Parametric Plot
#'
#' This function create box plots to report factorial non-parametric test.
#'
#' @param data a data.frame containing the data in which performing the ANOVA test
#' @param x a character string containing the name of variable used as x-axis
#' @param y a character string containing the name of variable used as y-axis
#' @param color a vector or character string containing the name of variable used as color in the box-plot
#' @param non the non-parametric statistical results returned by rstatix
#' @param pwc the statistical results returned by a pairwise comparisons between groups from rstatix
#' @param linetype the character string indicating the column in data for changing linetype
#' @param by the character vector containing the columns in the data to be used as variable to generate the grouping panels
#' @param addParam the character vector with elements to be included in the plot (e.g. "dotplot", "jitter", "boxplot", "point", "mean", "mean_se", "mean_sd", "mean_ci", "mean_range", "median", "median_iqr", "median_mad", "median_range"); see ?desc_statby for more details.
#' @param font.label.size the integer value with the font label size
#' @param step.increase the numeric vector to be used to minimize the overlap
#' @return A ggplot object with the non.parametric plot
#' @export
ggPlotFactNonParam <- function(data, x, y, color = c(), non, pwc, linetype = color, by = c(), addParam = c(), font.label.size = 14, step.increase = 0.25, type = NULL) {

  data[[x]] <- factor(data[[x]])
  pwc2 <- tryCatch(rstatix::add_xy_position(pwc, x = x, step.increase = step.increase), error = function(e) NULL)
  if (is.null(pwc2)) return(ggplot2::ggplot())
  if (length(color) > 0) {
    bxp <- ggpubr::ggboxplot(data, x = x, y = y, color = color, palette = "jco", add=addParam, facet.by = by)
    bxp1 <- bxp + ggpubr::stat_pvalue_manual(pwc2, color = color, linetype = linetype, hide.ns = T, tip.length = 0, step.group.by = by)
    ggtest <- tryCatch(ggplot2::ggplot_build(bxp1), error = function(e) NULL)
    if (!is.null(ggtest)) bxp <- bxp1
  } else {
    bxp <- ggpubr::ggboxplot(data, x = x, y = y, color = x, palette = "jco", add=addParam, facet.by = by)
    bxp1 <- bxp + ggpubr::stat_pvalue_manual(pwc2, linetype = linetype, hide.ns = T, tip.length = 0, step.group.by = by)
    ggtest <- tryCatch(ggplot2::ggplot_build(bxp1), error = function(e) NULL)
    if (!is.null(ggtest)) bxp <- bxp1
  }

  if (!is.null(type) && 'srh' == type) {
    dnon <- data.frame(non)
    idx <- which(rownames(dnon) == x)
    statistic <- round(as.double(dnon$H[idx]), 3)
    dof <- floor(as.double(dnon[['Df']][idx]))
    dof.res <- floor(as.double(dnon[['Df']][which(rownames(dnon) == 'Residuals')]))
    p <- round(as.double(dnon[['p.value']][idx]), 3)
    pval <- ifelse(p < 0.001, paste0(' , p<0.001'), paste0(' , p=', p))

    bxp <- bxp + ggplot2::labs(
      subtitle = paste0('Scheirer-Ray-Hare H(',dof,',',dof.res,')=', statistic, pval), caption = rstatix::get_pwc_label(pwc2))
  } else {
    bxp <- bxp + ggplot2::labs(subtitle = rstatix::get_test_label(non, detailed = T), caption = rstatix::get_pwc_label(pwc2))
  }
  bxp <- bxp + ggplot2::theme(text = ggplot2::element_text(size=font.label.size))

  #attr(bxp,"lbl") <- paste0(
  #  'Plot of "',y,'" based on "',x,'"',
  #  ifelse(length(by)>0, paste0(' grouped by "',paste0(by,collapse='*')),'"'),
  #  ifelse(length(color)>0, paste0(' (color: ',color,')'), '')
  #)
  return(bxp)
}


#' Three-Way Factorial Non Parametric Plots
#'
#' This functions returns a list of Box Plots related to Non Parametric test.
#'
#' @param data a data.frame containing the data in which performing the Three-Way Non Parametric test
#' @param dv a character string containing the variable used as y-axis
#' @param ivs a character vector containing the variables used as x-axis in Three-Way Non Parametric test
#' @param non the Non Parametric statistical results returned by rstatix::anova_test
#' @param pwcs the list of statistical results returned from pairwise comparisons between groups from rstatix
#' @param addParam the character vector with elements to be included in the plot (e.g. "dotplot", "jitter", "boxplot", "point", "mean", "mean_se", "mean_sd", "mean_ci", "mean_range", "median", "median_iqr", "median_mad", "median_range"); see ?desc_statby for more details.
#' @param font.label.size the integer value with the font label size
#' @param step.increase the numeric vector to be used to minimize the overlap
#' @return A list of ggplot objects with the Three-Way Non Parametric plots
#' @export
threeWayNonParamFactPlots <- function(data, dv, ivs, non, pwcs, addParam=c(), font.label.size = 14, step.increase = 0.25, type = NULL) {
  livs <- as.list(ivs); names(livs) <- ivs
  toReturn <- lapply(livs, FUN = function(iv) {
    pwc <- pwcs[[iv]]
    gbys <- setdiff(ivs, iv)
    lgbys <- as.list(gbys); names(lgbys) <- gbys
    inner.toReturn <- lapply(lgbys, FUN = function(gby) {
      color <- setdiff(ivs, c(iv, gby))
      ggPlotFactNonParam(data, iv, dv, color=color, by=gby, non=non, pwc=pwc, addParam=addParam,
                         font.label.size = font.label.size, step.increase = step.increase, type = type)
    })
    return(inner.toReturn)
  })
  return(toReturn)
}

#' Two-Way Factorial Non Parametric Plots
#'
#' This functions returns a list of Box Plots .
#'
#' @param data a data.frame containing the data in which performing the Two-Way  test
#' @param dv a character string containing the variable used as y-axis
#' @param ivs a character vector containing the variables used as x-axis in Two-Way  test
#' @param non the ANOVA statistical results returned by rstatix::anova_test
#' @param pwcs the list of statistical results returned from pairwise comparisons between groups from rstatix
#' @param addParam the character vector with elements to be included in the plot (e.g. "dotplot", "jitter", "boxplot", "point", "mean", "mean_se", "mean_sd", "mean_ci", "mean_range", "median", "median_iqr", "median_mad", "median_range"); see ?desc_statby for more details.
#' @param font.label.size the integer value with the font label size
#' @param step.increase the numeric vector to be used to minimize the overlap
#' @return A list of ggplot objects with the Two-Way  plots
#' @export
twoWayNonParamFactPlots <- function(data, dv, ivs, non, pwcs, addParam=c(), font.label.size = 14, step.increase = 0.25, type = NULL) {
  livs <- as.list(ivs); names(livs) <- ivs
  return(lapply(livs, FUN = function(iv) {
    pwc <- pwcs[[iv]]
    color <- setdiff(ivs, iv)
    ggPlotFactNonParam(data, iv, dv, color=color, non=non, pwc=pwc, addParam=addParam,
              font.label.size = font.label.size, step.increase = step.increase, type = type)
  }))
}

#' One-Way  Plots
#'
#' This functions returns a list of Box Plots related to One-Way  test.
#'
#' @param data a data.frame containing the data in which performing the One-Way  test
#' @param dv a character string containing the variable used as y-axis
#' @param ivs a character vector containing the variables used as x-axis in One-Way  test
#' @param non the non-parametric statistical results returned by rstatix::anova_test
#' @param pwcs the list of statistical results returned from pairwise comparisons between groups from rstatix
#' @param addParam the character vector with elements to be included in the plot (e.g. "dotplot", "jitter", "boxplot", "point", "mean", "mean_se", "mean_sd", "mean_ci", "mean_range", "median", "median_iqr", "median_mad", "median_range"); see ?desc_statby for more details.
#' @param font.label.size the integer value with the font label size
#' @param step.increase the numeric vector to be used to minimize the overlap
#' @return A list of ggplot objects with the One-Way  plots
#' @export
oneWayNonParamFactPlots <- function(data, dv, ivs, non, pwcs, addParam=c(), font.label.size = 14, step.increase = 0.25, type = NULL) {
  livs <- as.list(ivs); names(livs) <- ivs
  return(lapply(livs, FUN = function(iv) {
    ggPlotFactNonParam(data, iv, dv, non=non, pwc=pwcs[[iv]], addParam=addParam,
              font.label.size = font.label.size, step.increase = step.increase, type = type)
  }))
}

