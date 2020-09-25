

#' @export
get.wilcoxon.pwc.table <- function(pwcs, only.sig = F) {
  cnames <- c("var")
  toReturn <- do.call(rbind, lapply(names(pwcs), FUN = function(dv) {
    do.call(rbind, lapply(names(pwcs[[dv]]), FUN = function(iv) {
      cnames <<- c(cnames, iv)
      pwc <- pwcs[[dv]][[iv]]
      pdat <- rstatix::add_significance(pwc)
      if (!is.null(pdat) && nrow(pdat) > 0) {
        pdat[[iv]] <- NA
        return(cbind(var = dv, pdat))
      }
    }))
  }))
  if (only.sig) {
    idx <- toReturn[["p.adj.signif"]] == "*"
    idx <- idx | toReturn[["p.adj.signif"]] == "**"
    idx <- idx | toReturn[["p.adj.signif"]] == "***"
    idx <- idx | toReturn[["p.adj.signif"]] == "****"
    idx <- idx | toReturn[["p.adj.signif"]] == "*****"
    toReturn <- toReturn[which(idx),]
  }
  cnames <- cnames[cnames %in% colnames(toReturn)]
  cnames <- unique(c(cnames, colnames(toReturn)))
  return(toReturn[,cnames])
}

#' Export as text a pairwise comparisons using Wilcoxon pairwise method
#'
#' @export
wilcoxon.pwc.as.text <- function(pwcs, ds, between, p.adjust.method = "bonferroni", lang='en') {
  pwc.df <- get.wilcoxon.pwc.table(pwcs, only.sig = T)
  if (nrow(pwc.df) > 0) {
    pwc.str <- c()
    for (i in seq(1,nrow(pwc.df))) {
      dv <- pwc.df$var[i]
      iv <- between
      if (length(between) > 1)
        iv <- names(pwc.df[i,between])[is.na(pwc.df[i,between])]

      dv.var <- 'var'
      if (!dv.var %in% colnames(ds)) dv.var <- 'variable'
      idx <- (ds[[dv.var]] == dv)
      lapply(setdiff(between, iv), FUN = function(iv2) {
        idx <<- idx & ds[[iv2]] == pwc.df[[iv2]][i]
      })

      ds.str <- c()
      for (val in as.character(pwc.df[i, c('group1','group2')])) {
        val <- as.character(val)
        mdn0 <- round(ds$median[which(idx & ds[[iv]] == val)],3)
        iqr0 <- round(ds[['iqr']][which(idx & ds[[iv]] == val)],3)
        ds.str <- c(
          ds.str,
          paste0(ifelse(lang=='pt','a mediana no','the median in the'),' ',
                 iv,'="',val,'" (Mdn=',mdn0,' ',ifelse(lang=='pt','e','and'),' IQR=',iqr0,')'))
      }

      ds.str <- paste0(ds.str,
                        collapse = ifelse(lang=='pt',
                                          ' foi significativamente diferente do que ',
                                          ' was significantly different than '))
      ds.str <- paste0(ds.str, ' ',ifelse(lang=='pt','com','with'),' p-adj',p.val.as.text(pwc.df$p.adj[i]))
      pwc.str <- c(pwc.str, ds.str)
    }
    pwc.str <- paste0(ifelse(lang=='pt','Para a variável dependente','For the dependent variable'),' ',
                      '"',dv,'", ', paste0(pwc.str, collapse='; '),'.')

    if(lang=='pt')
      toReturn <- paste0(
        'Comparações emparelhadas usando testes Wilcoxon ',
        'foram computadas para encontrar diferenças estatítiscamente significativas entre os grupos ',
        'definidos pelas variáveis independentes e com os p-values ajustado pelo método ',
        '"',p.adjust.method,'". ', pwc.str,"\n\n")
    else
      toReturn <- paste0(
        'Pairwise comparisons using Wilcoxon Tests ',
        'were computed to find statistically significant diferences among the groups ',
        'defined by the independent variables, and with the p-values ajusted by the method ',
        '"',p.adjust.method,'". ', pwc.str,"\n\n")

    return(toReturn)
  }
}
