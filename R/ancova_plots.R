#' @export
ggPlotAoC <- function(data, y, x, color = c(), aov, pwc, line.color = color, font.label.size = 12, step.increase = 0.25) {
  data[[x]] <- factor(data[[x]])
  pwc2 <- tryCatch(rstatix::add_xy_position(pwc, x=x, fun="mean_se", step.increase=step.increase), error = function(e) NULL)

  if (is.null(pwc2)) return(ggplot2::ggplot())

  emms <- rstatix::get_emmeans(pwc2)
  if (nrow(emms) > 2) {
    lp <- ggpubr::ggline(emms, x=x, y="emmean", color=line.color, palette = "jco", plot_type='b', size=0.4)
    lp <- lp + ggplot2::geom_errorbar(ggplot2::aes_string(ymin="conf.low", ymax="conf.high", color=color), width=0.1, size = 1)
  } else {
    lp <- ggpubr::ggline(emms, x=x, y="emmean", palette = "jco", plot_type='b', size=0.4)
    lp <- lp + ggplot2::geom_errorbar(ggplot2::aes_string(ymin="conf.low", ymax="conf.high"), width=0.1, size = 1)
  }

  lp <- lp + ggpubr::stat_pvalue_manual(pwc2, hide.ns=T, tip.length=0.005, bracket.size=0.3)
  lp <- lp + ggplot2::labs(subtitle = rstatix::get_test_label(aov, detailed=T), caption=rstatix::get_pwc_label(pwc))
  lp <- lp + ggplot2::theme(text = ggplot2::element_text(size=font.label.size))

  lp <- lp + ggplot2::geom_jitter(data = data, ggplot2::aes_(x=as.name(x),y=as.name(y),colour=factor(data[[color]])),width=0.075,height=0.075,size=0.75)
  return(lp)
}

#' One-Way ANCOVA Plots
#'
#' This functions returns a list of Plots related to one-Way ANCOVA test.
#'
#' @param data a data.frame containing the data in which performing the one-Way ANOCVA test
#' @param dv a character string containing the variable used as y-axis
#' @param ivs a character vector containing the variables used as x-axis in one-Way ANCOVA test
#' @param aov the ANCOVA statistical results returned by rstatix::anova_test
#' @param pwcs the list of statistical results returned from pairwise comparisons between groups from rstatix
#' @param font.label.size the integer value with the font label size
#' @param step.increase the numeric vector to be used to minimize the overlap
#' @return A list of ggplot objects with the Two-Way ANOVA plots
#' @export
oneWayAncovaPlots <- function(data, dv, ivs, aov, pwcs, font.label.size = 12, step.increase = 0.25) {
  livs <- as.list(ivs); names(livs) <- ivs
  return(lapply(livs, FUN = function(iv) {
    pwc <- pwcs[[iv]]
    color <- iv
    lp <- ggPlotAoC(data, dv, iv, color, aov, pwc, "black", font.label.size, step.increase)
    return(lp)
  }))
}


#' Two-Way ANCOVA Plots
#'
#' This functions returns a list of Plots related to Two-Way ANCOVA test.
#'
#' @param data a data.frame containing the data in which performing the Two-Way ANCOVA test
#' @param dv a character string containing the variable used as y-axis
#' @param ivs a character vector containing the variables used as x-axis in Two-Way ANCOVA test
#' @param aov the ANCOVA statistical results returned by rstatix::anova_test
#' @param pwcs the list of statistical results returned from pairwise comparisons between groups from rstatix
#' @param font.label.size the integer value with the font label size
#' @param step.increase the numeric vector to be used to minimize the overlap
#' @return A list of ggplot objects with the Two-Way ANOVA plots
#' @export
twoWayAncovaPlots <- function(data, dv, ivs, aov, pwcs, font.label.size = 12, step.increase = 0.25) {
  livs <- as.list(ivs); names(livs) <- ivs
  return(lapply(livs, FUN = function(iv) {
    pwc <- pwcs[[iv]]
    color <- setdiff(ivs, iv)
    lp <- ggPlotAoC(data, dv, iv, color, aov, pwc, color, font.label.size, step.increase)
    return(lp)
  }))
}
