#' @export
density_res_plot <- function(data, dv, between = c(), within = c(), covar = NULL, dv.var = NULL, bins = 35) {
  if (is.data.frame(data)) {
    dat <- as.data.frame(data)
    if (!is.null(dv.var))
      dat <- as.data.frame(data[data[[dv.var]] == dv,])
  } else if (is.list(data)) {
    dat <- as.data.frame(data[[dv]])
  }

  sformula <- as_formula(dv, between, within, covar)
  res <- residuals(lm(sformula, data = dat))
  gplot <- ggpubr::gghistogram(
    data.frame(wid = names(res), res = res), x = "res", y = "..density..", add = "mean",
    bins = bins, palette = "jco", rug = T, add_density = T)
  gplot <- gplot + ggpubr::stat_overlay_normal_density(color = "red", linetype = "dashed")
  return(gplot)
}

#' @import shiny
qqResidualPanel <- function(data, dv, wid = 'row.pos', between = c(), within = c(),
                            covar = NULL, dv.var = NULL, width = 500, height = 400, bins = 30) {
  dat <- as.data.frame(data)
  ivs <- c(between, within)
  if (!is.null(dv.var))
    dat <- data[data[[dv.var]] == dv,]

  sformula <- as_formula(dv, between, within, covar, wid, as.character = T)
  if (length(ivs) == 0 && length(covar) == 0) {
    res <- dat[[dv]]
    names(res) <- dat[[wid]]
  } else if (length(within) > 0) {
    res <- as.data.frame(stats::proj(stats::aov(as.formula(sformula), data = dat))[[3]])$Residuals
    names(res) <- dat[[wid]]
  } else {
    res <- residuals(lm(as.formula(sformula), data = dat))
    names(res) <- dat[[wid]]
  }

  verticalLayout(
   h4(paste0("Residual Model: ", sformula))
  ,splitLayout(
    cellWidths = width,
    tabsetPanel(
      type = "pills",
      tabPanel(
        paste0("QQ-Classical"),
        renderPlot({
          car::qqPlot(res, ylab = "sample")
        }, width = width, height = height)
      ),
      tabPanel(
        paste0("QQ-Interactive"),
        plotly::renderPlotly({
          plotly::layout(qqPlotly(res), width = width, height = height)
        })
      )
    ),
    tabsetPanel(
      type = "pills",
      tabPanel(
        paste0("Histogram"),
        renderPlot({
          gplot <- ggpubr::gghistogram(
            data.frame(wid = names(res), res = res), x = "res", y = "..density..", add = "mean",
            bins = bins, palette = "jco", rug = T, add_density = T)
          gplot <- gplot + ggpubr::stat_overlay_normal_density(color = "red", linetype = "dashed")
          gplot
        }, width = width, height = height)
      ),
      tabPanel(paste0("Skewness & Kurtosis"), renderPrint({ symmetry_test(res) }))
    )
  )
  )
}
