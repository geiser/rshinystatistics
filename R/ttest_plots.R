#' Independent T-Test Plot
#'
#' This function create ggpubr box-plot to report result from an independent t-test.
#'
#' @param data a data.frame containing the data in which performing the t-test
#' @param x a character string containing the name of variable used as x-axis
#' @param y a character string containing the name of variable used as y-axis
#' @param tt the statistical results returned by a t_test from rstatix
#' @param addParam the character vector with elements to be included in the plot (e.g. "dotplot", "jitter", "boxplot", "point", "mean", "mean_se", "mean_sd", "mean_ci", "mean_range", "median", "median_iqr", "median_mad", "median_range"); see ?desc_statby for more details.
#' @param font.label.size the integer value with the font label size
#' @return A ggplot object with the t-test plot
#' @export
ggPlotTTest <- function(data, x, y, tt, addParam = c(), font.label.size = 14) {
  stat.test <- rstatix::add_xy_position(rstatix::add_significance(tt), x=x, fun="max", step.increase = 0.25)
  bxp <- ggpubr::ggboxplot(
    data, x=x, y=y, color=x, width=0.5, add=addParam, palette="jco"
  )
  bxp <- bxp + ggpubr::stat_pvalue_manual(stat.test, hide.ns=T, tip.length = 0)
  bxp <- bxp + ggplot2::labs(subtitle = rstatix::get_test_label(stat.test, detailed=T))
  bxp <- bxp + ggplot2::theme(text = ggplot2::element_text(size=font.label.size))
  return(bxp)
}

#' Paired T-Test Plot
#'
#' This function create ggpubr box-plot to report result from a paired t-test.
#'
#' @param data a data.frame containing the data in which performing the t-test
#' @param x a character string containing the name of variable used as x-axis
#' @param y a character string containing the name of variable used as y-axis
#' @param tt the statistical results returned by a t_test from rstatix
#' @param id the identification column of observations in the data
#' @param font.label.size the integer value with the font label size
#' @return A ggplot object with the t-test plot
#' @export
ggPlotPairedTTest <- function(data, x, y, tt, id, font.label.size = 14) {
  stat.test <- rstatix::add_xy_position(rstatix::add_significance(tt), x=x)
  bxp <- ggpubr::ggpaired(
    data, x=x, y=y, color=x, width=0.5, id=id, palette="jco",line.size=0.1, line.color = "gray"
  )
  bxp <- bxp + ggpubr::stat_pvalue_manual(stat.test, hide.ns=T, tip.length = 0)
  bxp <- bxp + ggplot2::labs(subtitle = rstatix::get_test_label(stat.test, detailed=T))
  bxp <- bxp + ggplot2::theme(text = ggplot2::element_text(size=font.label.size))
  return(bxp)
}
