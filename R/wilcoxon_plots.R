#' Non Parametric Wilcoxon Plot
#'
#' @export
ggPlotWilcoxon <- function(data, x, y, wt, addParam = c(), font.label.size = 14) {
  stat.test <- rstatix::add_xy_position(rstatix::add_significance(wt), x=x, fun="max", scales = "free")
  bxp <- ggpubr::ggboxplot(
    data, x=x, y=y, color=x, width=0.5, add=addParam, palette="jco"
  )
  bxp <- bxp + ggpubr::stat_pvalue_manual(stat.test, hide.ns=T, tip.length = 0)
  bxp <- bxp + ggplot2::labs(subtitle = rstatix::get_test_label(stat.test, detailed=T))
  bxp <- bxp + ggplot2::theme(text = ggplot2::element_text(size=font.label.size))
  return(bxp)
}
