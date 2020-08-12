#' T-Test Plot
#'
#' This function create box plots to report results from t-Test.
#'
#' @param data a data.frame containing the data in which performing the t-test
#' @param x a character string containing the name of variable used as x-axis
#' @param y a character string containing the name of variable used as y-axis
#' @param tt the statistical results returned by a t_test from rstatix
#' @param addParam the character vector with elements to be included in the plot (e.g. "dotplot", "jitter", "boxplot", "point", "mean", "mean_se", "mean_sd", "mean_ci", "mean_range", "median", "median_iqr", "median_mad", "median_range"); see ?desc_statby for more details.
#' @param font.label.size the integer value with the font label size
#' @return A ggplot object with the t-test plot
#' @export
ggPlotTTest <- function(data, x, y, tt, addParam = c(), font.label.size = 10) {
  stat.test <- add_xy_position(add_significance(tt), x=x, step.increase = 0.005)
  bxp <- ggboxplot(
    data, x=x, y=y, color=x, width=0.5, add=addParam, palette="jco"
  )
  bxp <- bxp + stat_pvalue_manual(stat.test, tip.length = 0)
  bxp <- bxp + labs(subtitle = get_test_label(stat.test, detailed=T))
  bxp <- bxp + theme(text = element_text(size=font.label.size))
  return(bxp)
}


#' Independent T-Test Plots
#'
#' This functions returns a list of Box Plots related to T-Test.
#'
#' @param data a data.frame containing the data in which performing the T-Test
#' @param dvs a character vector containing the dependent variables used as y-axis
#' @param iv a character vector containing the independent variable used as x-axis
#' @param tts the statistical results returned by rstatix::t_test
#' @param addParam the character vector with elements to be included in the plot (e.g. "dotplot", "jitter", "boxplot", "point", "mean", "mean_se", "mean_sd", "mean_ci", "mean_range", "median", "median_iqr", "median_mad", "median_range"); see ?desc_statby for more details.
#' @param font.label.size the integer value with the font label size
#' @param dv.var column with the information to classify observations based on dependent variables
#' @return A list of ggplot objects with the T-Test plots
#' @export
tTestPlots <- function(data, dvs, iv, tts, addParam=c(), font.label.size = 10, dv.var = NULL) {
  dat <- data
  ldvs <- as.list(dvs); names(dvs) <- dvs
  return(lapply(ldvs, FUN = function(dv) {
    if (!is.null(dv.var)) dat <- as.data.frame(dat[which(dat[[dv.var]] == dv),])
    ggPlotTTest(data, iv, dv, tts[[dv]], addParam = addParam, font.label.size = font.label.size)
  }))
}
