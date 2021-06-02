#' @export
density_grp_plot <- function(data, dv, wid ='row.pos', ids = data[[wid]], name = paste0('All of ',dv), dv.var = NULL, bins = 35) {
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

#' @import shiny
qqGroupPanel <- function(data, dv, wid = 'row.pos', ids = data[[wid]], name = paste0('All of ', dv),
                         dv.var = NULL, width = 500, height = 400, bins = 30) {
  if (is.data.frame(data)) {
    df <- as.data.frame(data)
    if (!is.null(dv.var))
      df <- df[df[[dv.var]] == dv,]
    df <- df[df[[wid]] %in% ids,]
  } else if (is.list(data)) {
    df <- data[[dv]]
  }

  values <- df[[dv]]
  names(values) <- df[[wid]]

  verticalLayout(
   h4(paste0("Group: ", name))
  , splitLayout(
    cellWidths = width,
    tabsetPanel(
      type = "pills",
      tabPanel(
        paste0("QQ-Classical"),
        renderPlot({
          car::qqPlot(values, ylab = "sample")
        }, width = width, height = height)
      ),
      tabPanel(
        paste0("QQ-Interactive"),
        plotly::renderPlotly({
          plotly::layout(qqPlotly(values), width = width, height = height)
        })
      )
    ),
    tabsetPanel(
      type = "pills",
      tabPanel(
        paste0("Histogram"),
        renderPlot({
          gplot <- ggpubr::gghistogram(
            df, x = dv, y = "..density..", add = "mean",
            bins = bins, palette = "jco", rug = T, add_density = T)
          gplot <- gplot + ggpubr::stat_overlay_normal_density(color = "red", linetype = "dashed")
          gplot
        }, width = width, height = height)
      ),
      tabPanel(paste0("Skewness & Kurtosis"), renderPrint({ symmetry_test(values) }))
    )
  )
  )
}

#' @export
info_for_qq_groups <- function(data, dv, ivs, wid = 'row.pos', dv.var = NULL) {
  if (is.data.frame(data)) {
    dat <- as.data.frame(data)
    if (!is.null(dv.var))
      dat <- as.data.frame(data[data[[dv.var]] == dv,])
  } else if (is.list(data)) {
    dat <- as.data.frame(data[[dv]])
  }

  sivs <- unique(ivs[ivs %in% colnames(dat)])

  toReturn <- list()
  freq_df <- subset(rstatix::freq_table(dat, vars = sivs), n >= 3)
  for (i in seq(1,nrow(freq_df))) {
    tbl <- freq_df[i,c(sivs)]
    df <- subset_by_tbl(dat, tbl, group = sivs)
    df <- dplyr::group_by_at(df, dplyr::vars(sivs))

    lbl <- paste0(paste0(tbl,collapse=':'),' (',paste0(names(tbl), collapse=':'),')')

    non.normal <- getNonNormal(df[[dv]], df[[wid]])
    toReturn[[lbl]] <- list(lbl = lbl, data = df, non.normal = non.normal, i = i)
  }
  return(toReturn)
}

