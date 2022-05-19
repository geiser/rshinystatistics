#' Wilcoxon Test
#'
#' This function provides a wrapper for rstatix::wilcox_test for the dependent variables `dvs`
#' including their effect sizes
#'
#' @param data a data.frame or list containing the data
#' @param dvs numeric columns with the dependent variables to be used in the wilcoxon-test
#' @param iv independent (between) variable in which perform the wilcoxon-test
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param dv.var column with the information to classify observations
#' @param as.list logical value indicating that the result should be returned as list indicating by separate the wilcoxon.test and effect size
#' @return A data.frame containing the results for the wilcoxon test and their effect sizes
#' @export
wilcoxon.test <- function(data, dvs, iv, alternative = 'two.sided', dv.var = NULL, as.list = FALSE) {
  ldvs <- as.list(dvs); names(ldvs) <- dvs
  wt <- lapply(ldvs, FUN = function(dv) {
    if (is.data.frame(data)) {
      dat <- as.data.frame(data)
      if (!is.null(dv.var))
        dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
    } else if (is.list(data)) {
      dat <- as.data.frame(data[[dv]])
    }

    sformula <- as.formula(paste0('`',dv,'` ~ `',iv,'`'))
    return(rstatix::wilcox_test(dat, sformula, alternative = alternative, detailed = T))
  })
  ez <- lapply(ldvs, FUN = function(dv) {
    if (is.data.frame(data)) {
      dat <- as.data.frame(data)
      if (!is.null(dv.var))
        dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
    } else if (is.list(data)) {
      dat <- as.data.frame(data[[dv]])
    }

    sformula <- as.formula(paste0('`',dv,'` ~ `',iv,'`'))
    return(rstatix::wilcox_effsize(dat, sformula, alternative = alternative))
  })
  wilcoxon.test <- do.call(rbind, lapply(dvs, FUN = function(dv) {
    return(rstatix::add_significance(merge(wt[[dv]], ez[[dv]])))
  }))
  if (as.list) {
    return(list(wilcoxon.test = wilcoxon.test, wt = wt, ez = ez))
  } else {
    return(wilcoxon.test)
  }
}

#' Tabulate Wilcoxon's Pairwise Comparisons
#'
#' Creates table of pairwise comparisons based on results from Wilcoxon's test
#'
#' @param pwcs a data.frame with the results of Wilcoxon's pairwise comparisons
#' @param only.sig logical; if TRUE, only statistical significant results will be tabulated
#' @return A data.frame containing the pairwise comparisons
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


