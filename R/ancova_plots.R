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

  pwc = pwc[pwc$.y. == y,]

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
    subtitle = tryCatch(rstatix::get_test_label(aov, detailed = T, row = row), error = function(e) NULL)
    if (is.null(subtitle)) {
      p = ifelse(aov$p[row] < 0.01, "p < 0.01", paste0("p = ",round(aov$p[row],2)))
      subtitle = paste0("Ancova, F(",aov$DFn[row],", ",aov$DFd[row],")", " = ", round(aov$F[row], 2),
                        ", ", p, paste0(", eta^2 = ", round(aov$ges[row],2)))
    }
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

#' ANCOVA Bar Plot
#'
#' This function create bar plots to report results from ANCOVA (AoV) test.
#'
#' @param data a data.frame containing the data in which performing the ANOVA test
#' @param dv a column name of dependent variable
#' @param iv a column name of independent variable
#' @param aov the ANOVA statistical results returned by rstatix::anova_test
#' @param pwc the statistical results returned by a pairwise comparisons between groups from rstatix
#' @param covar a column name of covariance variable
#' @param pre.post a column name of time variable (pre- and post-test phases)
#' @param bar.width the numeric value for the bar plots
#' @param color a vector containing the colors in the box-plot
#' @param show.errorbar a boolean that indicates if the plot shows error bar
#' @param theme the theme used in the bar plot
#' @param font.size the list with the font label sizes
#' @param step.increase the numeric vector to be used to minimize the overlap
#' @param subtitle the subtitle in the plot, use number to indicate the row from ANCOVA table
#' @param ylim the number that indicates the axis-y limit
#' @export
ggBarPlotAoC <- function(data, dv, iv, aov, pwc, covar = NULL, pre.post = NULL,
                         bar.width = 0.75, color = c(), show.errorbar = T, theme = c(),
                         font.size = list(text.x=10, text.y=10, title.x=12, title.y=12),
                         step.increase = 0.2, subtitle = c(), ylim = NA) {
  if (is.null(aov) || is.null(pwc)) return(NULL)
  if (is.null(covar) && is.null(pre.post)) return(NULL)

  if (!is.null(covar)) {
    df <- get.descriptives(data, c(dv, covar), iv)
    gg1 <- ggplot2::ggplot(data=df, aes(x=variable, y=mean, fill=.data[[iv]]))

    ngroup = length(unique((df[[iv]])))
    xvars = unique(df$variable)
    sig.g.comb = combn(unique(df[[iv]]), 2, simplify = F)
    ycol = ".y."
  } else if (!is.null(pre.post)) {
    df <- get.descriptives(data, dv, c(pre.post, iv))
    gg1 <- ggplot2::ggplot(data=df, aes(x=.data[[iv]], y=mean, fill=.data[[pre.post]]))

    ngroup = length(unique((df[[pre.post]])))
    xvars = unique(df[[iv]])
    sig.g.comb = combn(unique(df[[pre.post]]), 2, simplify = F)
    ycol = iv
  }
  gg1 <- gg1 + ggplot2::geom_bar(stat="identity", position=position_dodge(), width = bar.width)

  if (show.errorbar)
    gg1 <- gg1 + ggplot2::geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1, position=position_dodge(bar.width))

  if (!is.null(color))
    gg1 <- gg1 + ggplot2::scale_fill_manual(values=color)

  if (!is.null(theme)) {
    if (theme == "gray")
      gg1 <- gg1 + ggplot2::theme_gray()
    if (theme == "linedraw")
      gg1 <- gg1 + ggplot2::theme_bw()
    if (theme == "light")
      gg1 <- gg1 + ggplot2::theme_light()
    if (theme == "dark")
      gg1 <- gg1 + ggplot2::theme_dark()
    if (theme == "minimal")
      gg1 <- gg1 + ggplot2::theme_minimal()
    if (theme == "classic")
      gg1 <- gg1 + ggplot2::theme_classic()
    if (theme == "void")
      gg1 <- gg1 + ggplot2::theme_void()
  }

  ylim1 = ylim
  for (i in 1:length(xvars)) {
    x = xvars[i]
    print(x)
    sig.pre.y = max(df$mean + df$sd, na.rm = F) + step.increase

    x.space = bar.width/ngroup
    sig.y = i-(bar.width/2)+(x.space/2)
    sig.x.comb = combn(seq(sig.y, by = x.space, length.out=ngroup), 2, simplify = F)

    for (j in 1:length(sig.x.comb)) {
      x1 = sig.x.comb[[j]][1]; x2 = sig.x.comb[[j]][2]
      g1 = sig.g.comb[[j]][1]; g2 = sig.g.comb[[j]][2]
      p <- pwc$p.adj[
        pwc[[ycol]] == x & ((pwc$group1 == g1 & pwc$group2 == g2)
                            | (pwc$group1 == g2 & pwc$group2 == g1))]
      if (!is.null(p) && !is.na(p) && length(p>0) && p < 0.05) {
        label = ifelse(p<=0.01, ifelse(p<=0.001, "***", "**"), "*")
        gg1 <- gg1 +
          ggplot2::geom_segment(x = x1, y = sig.pre.y, xend = x2, yend = sig.pre.y) +
          ggplot2::geom_text(x=(x1+x2)/2, y = sig.pre.y+0.02, label=label)
        sig.pre.y = sig.pre.y + step.increase
      }
    }

    if (is.na(ylim1) || ylim1 < sig.pre.y)
      ylim1 = sig.pre.y
  }
  gg1 <- gg1 + ylim(NA,ylim1)


  if (!is.null(subtitle) && is.numeric(subtitle)) {
    if (subtitle == 0) { row = which(aov$Effect == x) } else { row = subtitle }
    subtitle = tryCatch(rstatix::get_test_label(aov, detailed = T, row = row), error = function(e) NULL)
    if (is.null(subtitle)) {
      p = ifelse(aov$p[row] < 0.01, "p < 0.01", paste0("p = ",round(aov$p[row],2)))
      subtitle = paste0("Ancova, F(",aov$DFn[row],", ",aov$DFd[row],")", " = ", round(aov$F[row], 2),
                        ", ", p, paste0(", eta^2 = ", round(aov$ges[row],2)))
    }
  }

  gg1 <- gg1 + ggplot2::labs(subtitle = subtitle, caption = rstatix::get_pwc_label(pwc))

  return(gg1)
}

#' One-Way ANCOVA Bar Plot
#'
#' This function create bar plots to report results from one-way ANCOVA (AoV) test.
#'
#' @param data a data.frame containing the data in which performing the ANOVA test
#' @param dv a column name of dependent variable
#' @param ivs a list of columns that represents independent variables
#' @param aov the ANOVA statistical results returned by rstatix::anova_test
#' @param pwcs the statistical results returned by a pairwise comparisons between groups from rstatix
#' @param covar a column name of covariance variable
#' @param pre.post a column name of time variable (pre- and post-test phases)
#' @param bar.width the numeric value for the bar plots
#' @param color a vector containing the colors in the box-plot
#' @param show.errorbar a boolean that indicates if the plot shows error bar
#' @param theme the theme used in the bar plot
#' @param font.size the list with the font label sizes
#' @param step.increase the numeric vector to be used to minimize the overlap
#' @param subtitle the subtitle in the plot, use number to indicate the row from ANCOVA table
#' @param ylim the number that indicates the axis-y limit
#' @export
oneWayAncovaBarPlots <- function(data, dv, ivs, aov, pwcs, covar = c(), pre.post = c(),
                                 bar.width = 0.75, color = c(), show.errorbar = T, theme = c(),
                                 font.size = list(text.x=10, text.y=10, title.x=12, title.y=12),
                                 step.increase = 0.2, subtitle = c(), ylim = NA) {
  livs <- as.list(ivs); names(livs) <- ivs
  return(lapply(livs, FUN = function(iv) {
    pwc = pwcs
    if (is.list(pwcs) && is.data.frame(pwcs[[iv]])) pwc = pwcs[[iv]]
    ggBarPlotAoC(data, dv, iv, aov, pwc, covar, pre.post, bar.width, color,
                 show.errorbar, theme, font.size, step.increase, subtitle, ylim)
  }))
}


#' Two-Way ANCOVA Bar Plots
#'
#' This functions returns a list of Plots related to two-Way ANCOVA test.
#'
#' @param data a data.frame containing the data in which performing the ANOVA test
#' @param dv a column name of dependent variable
#' @param ivs a list of columns that represents independent variables
#' @param aov the ANOVA statistical results returned by rstatix::anova_test
#' @param pwcs the statistical results returned by a pairwise comparisons between groups from rstatix
#' @param covar a column name of covariance variable
#' @param pre.post a column name of time variable (pre- and post-test phases)
#' @param bar.width the numeric value for the bar plots
#' @param color a vector containing the colors in the box-plot
#' @param show.errorbar a boolean that indicates if the plot shows error bar
#' @param theme the theme used in the bar plot
#' @param font.size the list with the font label sizes
#' @param step.increase the numeric vector to be used to minimize the overlap
#' @param subtitle the subtitle in the plot, use number to indicate the row from ANCOVA table
#' @param ylim the number that indicates the axis-y limit
#' @export
twoWayAncovaBarPlots <- function(data, dv, ivs, aov, pwcs, covar=c(), pre.post = c(),
                                 bar.width = 0.75, color = c(), show.errorbar = T, theme = c(),
                                 font.size = list(text.x=10, text.y=10, title.x=12, title.y=12),
                                 step.increase = 0.2, subtitle = c(), ylim = NA) {

  pwc = pwcs
  if (!is.data.frame(pwcs)) {
    pwc <- do.call(rbind, lapply(ivs, FUN = function(iv) {
      pwc = pwcs[[iv]]
      if (iv == ivs[1]) {
        pwc$group1 = paste0(pwc$group1, ":" , pwc[[setdiff(ivs, iv)]])
        pwc$group2 = paste0(pwc$group2, ":" , pwc[[setdiff(ivs, iv)]])
      } else {
        pwc$group1 = paste0(pwc[[setdiff(ivs, iv)]], ":", pwc$group1)
        pwc$group2 = paste0(pwc[[setdiff(ivs, iv)]], ":", pwc$group2)
      }
      pwc = pwc[,-c(1)]
      return(pwc)
    }))
  } else {
    pwc[[paste0(ivs, collapse = ":")]] = apply(pwc[,ivs], 1, paste0, collapse=":")
  }

  data[[paste0(ivs, collapse = ":")]] = apply(data[,ivs], 1, paste0, collapse=":")

  toReturn = list()
  toReturn[[paste0(ivs, collapse = ":")]] = ggBarPlotAoC(
    data, dv, paste0(ivs, collapse = ":"), aov, pwc, covar, pre.post, bar.width,
    color, show.errorbar, theme, font.size, step.increase, subtitle, ylim)
  return(toReturn)
}

