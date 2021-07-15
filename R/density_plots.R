
#' @export
density.plot.by.residual <- function(data, dv, between = c(), within = c(), covar = NULL, dv.var = NULL, bins = 35) {
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


#' @export
density.plot <- function(data, dv, wid ='row.pos', ids = data[[wid]], name = paste0('All of ',dv), dv.var = NULL, bins = 35) {
  df <- as.data.frame(data)
  if (!is.null(dv.var))
    df <- df[df[[dv.var]] == dv,]
  df <- df[df[[wid]] %in% ids,]

  res <- df[[dv]]
  names(res) <- df[[wid]]

  gplot <- ggpubr::gghistogram(df, x = dv, y = "..density..", add = "mean", bins = bins, palette = "jco", rug = T, add_density = T)
  gplot <- gplot + ggpubr::stat_overlay_normal_density(color = "red", linetype = "dashed")
  return(gplot)
}
