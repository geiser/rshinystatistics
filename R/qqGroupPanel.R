#' @export
density_grp_plot <- function(data, dv, wid ='row.pos', ids = data[[wid]], name = paste0('All of ',dv), dv.var = NULL, bins = 35) {
  df <- as.data.frame(data)
  if (!is.null(dv.var))
    df <- df[df[[dv.var]] == dv,]
  df <- df[df[[wid]] %in% ids,]

  res <- df[[dv]]
  names(res) <- df[[wid]]

  gplot <- ggpubr::gghistogram(df, x = dv, y = "..density..", add = "mean",
    bins = bins, palette = "jco", rug = T, add_density = T)
  gplot <- gplot + ggpubr::stat_overlay_normal_density(color = "red", linetype = "dashed")
  return(gplot)
}

#' @import shiny
qqGroupPanel <- function(data, dv, wid = 'row.pos', ids = data[[wid]], name = paste0('All of ', dv),
                         dv.var = NULL, width = 500, height = 400, bins = 30) {
  df <- as.data.frame(data)
  if (!is.null(dv.var))
    df <- df[df[[dv.var]] == dv,]
  df <- df[df[[wid]] %in% ids,]

  values <- df[[dv]]
  names(values) <- df[[wid]]

  verticalLayout(
   h4(paste0("Group: ", name))
  ,splitLayout(
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
  dat <- as.data.frame(data)
  if (!is.null(dv.var))
    dat <- as.data.frame(data[data[[dv.var]] == dv,])

  toReturn <- list()
  freq_df <- subset(rstatix::freq_table(dat, vars = ivs), n >= 3)
  for (i in seq(1,nrow(freq_df))) {
    tbl <- freq_df[i,c(ivs)]
    df <- subset_by_tbl(dat, tbl, group = ivs)
    df <- dplyr::group_by_at(df, vars(ivs))
    lbl <- paste(sapply(names(tbl), FUN = function(nc) { paste0(nc,':', tbl[[nc]]) }), collapse = "-")
    non.normal <- getNonNormal(df[[dv]], df[[wid]])
    toReturn[[lbl]] <- list(lbl = lbl, data = df, non.normal = non.normal, i = i)
  }
  return(toReturn)
}

