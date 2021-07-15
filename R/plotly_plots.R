

#' @import stats
getQQline <- function(x) {
  data.quartiles <- quantile(x, c(0.25, 0.75))
  norm.quartiles <- qnorm(c(0.25, 0.75))
  b <- (data.quartiles[2] - data.quartiles[1])/(norm.quartiles[2] - norm.quartiles[1])
  a <- data.quartiles[1] - norm.quartiles[1] * b
  return(list(intercept = a, slope = b))
}

#' QQ plot in Plotly format
#'
#' @import plotly
#' @import stats
qqPlotly <- function(y, y.name = names(y)) {
  if (is.null(y.name))
    names(y) <- as.character(y)
  y <- sort(y)

  x.norm <- qqnorm(y, plot.it = F)
  qqline <- getQQline(y)

  plot_df <- as.data.frame(cbind(x=as.double(x.norm$x), y=as.double(x.norm$y), wid = names(y)))

  x0=seq(floor(as.numeric(min(x.norm$x)))-1.5, floor(as.numeric(max(x.norm$x)))+1.5)
  y0=(qqline$slope*x0)+as.numeric(qqline$intercept)

  p <- plotly::plot_ly()
  p <- plotly::add_trace(p, x=x0, y=y0, mode="lines", line=list(color='blue', width=1))
  p <- plotly::add_trace(p, data=plot_df, type='scatter', x=~x, y=~y, text=~wid
                         , marker = list(size=10, color='red', line = list(color='red', width=2)))
  p <- plotly::layout(p, showlegend=F
                      , xaxis = list(title = "theoretical", showline = T, zeroline = F, range=c(min(x0),max(x0)))
                      , yaxis = list(title = "sample", showline = T, zeroline = F, range=c(min(y0),min(y0))))
  return(p)
}

#' @import plotly
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
