#' @import plotly
#' @export
boxPlotly <- function(dat, dv, iv, wid = 'row.pos', boxpoints = "none", title = paste0(dv, ' ~ ', iv)) {
  p <- plotly::plot_ly(
    data=dat, type = "box", boxpoints = boxpoints,
    text=as.formula(paste0("~", '`', wid, '`')),
    x=as.formula(paste0("~",'`',iv,'`')),
    y=as.formula(paste0("~",'`',dv,'`')),
    color=as.formula(paste0("~",'`',iv,'`')))
  p <- plotly::layout(p, title = title, showlegend = F)
  return(p)
}

