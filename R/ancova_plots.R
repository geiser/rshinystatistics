#' ANCOVA Plot
#'
#' This function create box plots to report results from ANCOVA (AoV).
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
#' @param p.label the label used for p-values
#' @param subtitle the subtitle in the plot, use number to indicate the row from ANCOVA table
#' @export
ggPlotAoC <- function(data, x, y, color = c(), aov, pwc, linetype = color, by = c(), addParam = c(), font.label.size = 14, step.increase = 0.25, palette = "jco", p.label = "p.adj.signif", subtitle = c()) {
  if (is.null(aov) || is.null(pwc)) return(NULL)

  data[[x]] <- factor(data[[x]])
  pd <- ggplot2::position_dodge(width = 0.15)
  pwc2 <- tryCatch(rstatix::add_xy_position(pwc, x=x, fun="max", step.increase=step.increase), error = function(e) NULL)
  if (is.null(pwc2)) return(ggplot2::ggplot())
  if (length(color) > 0) {
    emms <- rstatix::get_emmeans(pwc2)
    lp <- ggpubr::ggline(emms, x=x, y = "emmean", color=color, palette = palette, plot_type='b', size=0.4, position = pd, facet.by = by, ylab = y)
    lp <- lp + ggplot2::geom_errorbar(ggplot2::aes_string(ymin="conf.low", ymax="conf.high", color=color), width=0.1, size = 1, position = pd)
    if (p.label == "p.adj") {
      pwc2[["p.adj"]] <- round(pwc2[["p.adj"]],3)
      pwc2$p.adj[which(pwc2$p.adj < 0.001)] <- '< 0.001'
      pwc2$p.adj[which(pwc2$p.adj >= 0.001)] <- paste0('= ', pwc2$p.adj[which(pwc2$p.adj >= 0.001)])
      lp <- lp + ggpubr::stat_pvalue_manual(pwc2, color = color, linetype = linetype, hide.ns = T, tip.length = 0, step.group.by = by, position = pd, label = "p.adj {p.adj}")
    } else {
      lp <- lp + ggpubr::stat_pvalue_manual(pwc2, color = color, linetype = linetype, hide.ns = T, tip.length = 0, step.group.by = by, position = pd)
    }
    if ('jitter' %in% addParam)
      lp <- lp + ggplot2::geom_jitter(data = data, ggplot2::aes_(x=as.name(x), y=as.name(y), colour=factor(data[[color]])), width=0.075, height=0.075, size=0.75)
  } else {
    emms <- rstatix::get_emmeans(pwc2)
    lp <- ggpubr::ggline(emms, x=x, y="emmean", color = "black", palette = palette, plot_type='b', size=0.4, position = pd, facet.by = by, ylab = y)
    lp <- lp + ggplot2::geom_errorbar(ggplot2::aes_string(ymin="conf.low", ymax="conf.high"), width=0.1, size = 1, position = pd)
    if (p.label == "p.adj") {
      pwc2[["p.adj"]] <- round(pwc2[["p.adj"]],3)
      pwc2$p.adj[which(pwc2$p.adj < 0.001)] <- '< 0.001'
      pwc2$p.adj[which(pwc2$p.adj >= 0.001)] <- paste0('= ', pwc2$p.adj[which(pwc2$p.adj >= 0.001)])
      lp <- lp + ggpubr::stat_pvalue_manual(pwc2, linetype = linetype, hide.ns = T, tip.length = 0, step.group.by = by, position = pd, label = "p.adj {p.adj}")
    } else {
      lp <- lp + ggpubr::stat_pvalue_manual(pwc2, linetype = linetype, hide.ns = T, tip.length = 0, step.group.by = by, position = pd)
    }
    if ('jitter' %in% addParam)
      lp <- lp + ggplot2::geom_jitter(data = data, ggplot2::aes_(x=as.name(x), y=as.name(y), colour=factor(data[[x]])), width=0.075, height=0.075, size=0.75)
  }

  if (!is.null(subtitle) && is.numeric(subtitle)) {
    if (subtitle == 0) { row = which(aov$Effect == x) } else { row = subtitle }
    subtitle = rstatix::get_test_label(aov, detailed = T, row = row)
  }

  lp <- lp + ggplot2::labs(subtitle = subtitle, caption = rstatix::get_pwc_label(pwc2))
  lp <- lp + ggplot2::theme(text = ggplot2::element_text(size=font.label.size))
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
#' @param p.label the expression used in the p-values in the plot
#' @param subtitle the subtitle in the plot, use number to indicate the row from ANCOVA table
#' @return A list of ggplot objects with the Two-Way ANOVA plots
#' @export
oneWayAncovaPlots <- function(data, dv, ivs, aov, pwcs, addParam=c(), font.label.size = 14, step.increase = 0.25, p.label = "p.adj.signif", subtitle = c()) {
  livs <- as.list(ivs); names(livs) <- ivs
  return(lapply(livs, FUN = function(iv) {
    ggPlotAoC(data, iv, dv, aov=aov, pwc=pwcs[[iv]], addParam=addParam,
              font.label.size = font.label.size, step.increase = step.increase, p.label = p.label, subtitle = subtitle)
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
#' @param p.label the expression used in the p-values in the plot
#' @param subtitle the subtitle in the plot, use number to indicate the row from ANCOVA table
#' @return A list of ggplot objects with the Two-Way ANCOVA plots
#' @export
twoWayAncovaPlots <- function(data, dv, ivs, aov, pwcs, addParam=c(), font.label.size = 14, step.increase = 0.25, p.label = "p.adj.signif", subtitle = c()) {
  livs <- as.list(ivs); names(livs) <- ivs
  return(lapply(livs, FUN = function(iv) {
    pwc <- pwcs[[iv]]
    color <- setdiff(ivs, iv)
    ggPlotAoC(data, iv, dv, color=color, aov=aov, pwc=pwc, addParam=addParam,
              font.label.size = font.label.size, step.increase = step.increase, p.label = p.label, subtitle = subtitle)
  }))
}

