#' ANOVA Plot
#'
#' This function create box plots to report results from ANOVA (AoV).
#'
#' @param data a data.frame containing the data in which performing the ANOVA test
#' @param x a character string containing the name of variable used as x-axis
#' @param y a character string containing the name of variable used as y-axis
#' @param color a vector or character string containing the name of variable used as color in the box-plot
#' @param aov the ANOVA statistical results returned by rstatix::anova_test
#' @param pwc the statistical results returned by a pairwise comparisons between groups from rstatix
#' @param linetype the character string indicating the column in data for changing linetype
#' @param by the character vector containing the columns in the data to be used as variable to generate the grouping panels
#' @param addParam the character vector with elements to be included in the plot (e.g. "dotplot", "jitter", "boxplot", "point", "mean", "mean_se", "mean_sd", "mean_ci", "mean_range", "median", "median_iqr", "median_mad", "median_range"); see ?desc_statby for more details.
#' @param font.label.size the integer value with the font label size
#' @param step.increase the numeric vector to be used to minimize the overlap
#' @return A ggplot object with the ANOVA plot
#' @export
ggPlotAoV <- function(data, x, y, color = c(), aov, pwc, linetype = color, by = c(), addParam = c(), font.label.size = 12, step.increase = 0.25) {
  if (is.null(aov) || is.null(pwc)) return(NULL)
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
  bxp <- bxp + ggplot2::labs(subtitle = rstatix::get_test_label(aov, detailed = T), caption = rstatix::get_pwc_label(pwc2))
  bxp <- bxp + ggplot2::theme(text = ggplot2::element_text(size=font.label.size))
  #attr(bxp,"lbl") <- paste0(
  #  'Plot of "',y,'" based on "',x,'"',
  #  ifelse(length(by)>0, paste0(' grouped by "',paste0(by,collapse='*')),'"'),
  #  ifelse(length(color)>0, paste0(' (color: ',color,')'), '')
  #)
  return(bxp)
}


#' Three-Way ANOVA Plots
#'
#' This functions returns a list of Box Plots related to Three-Way ANOVA test.
#'
#' @param data a data.frame containing the data in which performing the Three-Way ANOVA test
#' @param dv a character string containing the variable used as y-axis
#' @param ivs a character vector containing the variables used as x-axis in Three-Way ANOVA test
#' @param aov the ANOVA statistical results returned by rstatix::anova_test
#' @param pwcs the list of statistical results returned from pairwise comparisons between groups from rstatix
#' @param addParam the character vector with elements to be included in the plot (e.g. "dotplot", "jitter", "boxplot", "point", "mean", "mean_se", "mean_sd", "mean_ci", "mean_range", "median", "median_iqr", "median_mad", "median_range"); see ?desc_statby for more details.
#' @param font.label.size the integer value with the font label size
#' @param step.increase the numeric vector to be used to minimize the overlap
#' @return A list of ggplot objects with the Three-Way ANOVA plots
#' @export
threeWayAnovaPlots <- function(data, dv, ivs, aov, pwcs, addParam=c(), font.label.size = 12, step.increase = 0.25) {
  livs <- as.list(ivs); names(livs) <- ivs
  toReturn <- lapply(livs, FUN = function(iv) {
    pwc <- pwcs[[iv]]
    gbys <- setdiff(ivs, iv)
    lgbys <- as.list(gbys); names(lgbys) <- gbys
    inner.toReturn <- lapply(lgbys, FUN = function(gby) {
      color <- setdiff(ivs, c(iv, gby))
      ggPlotAoV(data, iv, dv, color=color, by=gby, aov=aov, pwc=pwc, addParam=addParam,
                font.label.size = font.label.size, step.increase = step.increase)
    })
    return(inner.toReturn)
  })
  return(toReturn)
}

#' Two-Way ANOVA Plots
#'
#' This functions returns a list of Box Plots related to Two-Way ANOVA test.
#'
#' @param data a data.frame containing the data in which performing the Two-Way ANOVA test
#' @param dv a character string containing the variable used as y-axis
#' @param ivs a character vector containing the variables used as x-axis in Two-Way ANOVA test
#' @param aov the ANOVA statistical results returned by rstatix::anova_test
#' @param pwcs the list of statistical results returned from pairwise comparisons between groups from rstatix
#' @param addParam the character vector with elements to be included in the plot (e.g. "dotplot", "jitter", "boxplot", "point", "mean", "mean_se", "mean_sd", "mean_ci", "mean_range", "median", "median_iqr", "median_mad", "median_range"); see ?desc_statby for more details.
#' @param font.label.size the integer value with the font label size
#' @param step.increase the numeric vector to be used to minimize the overlap
#' @return A list of ggplot objects with the Two-Way ANOVA plots
#' @export
twoWayAnovaPlots <- function(data, dv, ivs, aov, pwcs, addParam=c(), font.label.size = 12, step.increase = 0.25) {
  livs <- as.list(ivs); names(livs) <- ivs
  return(lapply(livs, FUN = function(iv) {
    pwc <- pwcs[[iv]]
    color <- setdiff(ivs, iv)
    ggPlotAoV(data, iv, dv, color=color, aov=aov, pwc=pwc, addParam=addParam,
              font.label.size = font.label.size, step.increase = step.increase)
  }))
}

#' One-Way ANOVA Plots
#'
#' This functions returns a list of Box Plots related to One-Way ANOVA test.
#'
#' @param data a data.frame containing the data in which performing the One-Way ANOVA test
#' @param dv a character string containing the variable used as y-axis
#' @param ivs a character vector containing the variables used as x-axis in One-Way ANOVA test
#' @param aov the ANOVA statistical results returned by rstatix::anova_test
#' @param pwcs the list of statistical results returned from pairwise comparisons between groups from rstatix
#' @param addParam the character vector with elements to be included in the plot (e.g. "dotplot", "jitter", "boxplot", "point", "mean", "mean_se", "mean_sd", "mean_ci", "mean_range", "median", "median_iqr", "median_mad", "median_range"); see ?desc_statby for more details.
#' @param font.label.size the integer value with the font label size
#' @param step.increase the numeric vector to be used to minimize the overlap
#' @return A list of ggplot objects with the One-Way ANOVA plots
#' @export
oneWayAnovaPlots <- function(data, dv, ivs, aov, pwcs, addParam=c(), font.label.size = 12, step.increase = 0.25) {
  livs <- as.list(ivs); names(livs) <- ivs
  return(lapply(livs, FUN = function(iv) {
    ggPlotAoV(data, iv, dv, aov=aov, pwc=pwcs[[iv]], addParam=addParam,
              font.label.size = font.label.size, step.increase = step.increase)
  }))
}

