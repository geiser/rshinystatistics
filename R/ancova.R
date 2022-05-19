#' ANCOVA Test
#'
#' This function provides a wrapper for rstatix::anova_test for the dependent variables `dvs`
#' including their effect sizes
#'
#' @param data a data.frame or list containing the data
#' @param dvs numeric columns with the dependent variables to be used in the anova test
#' @param between independent (between) variable in which perform the anova test
#' @param covar column with the covariate information
#' @param within independent (within) variable in which perform the anova test
#' @param wid column with the unique identification
#' @param type the type of sums of squares for ANOVA. Allowed values are either 1, 2 or 3. Default value is 2.
#' @param effect.size the effect size to be computed, being "ges" (generalized eta squared) or "pes" (partial eta squared) Default is "ges".
#' @param dv.var column with the information to classify observations
#' @param as.table logical value indicating that the result should be returned after to apply `get.anova.table` function
#' @param skewness named list in which each name represent the column of skewness data
#' @return A data.frame containing the results for the anova test and their effect sizes
#' @export
ancova.test <- function(data, dvs, between, covar, type, effect.size, dv.var = NULL, as.table = F) {
  ldvs <- as.list(dvs); names(ldvs) <- dvs
  toReturn <- lapply(ldvs, FUN = function(dv) {
    if (is.data.frame(data)) {
      dat <- as.data.frame(data)
      if (!is.null(dv.var))
        dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
    } else if (is.list(data)) {
      dat <- as.data.frame(data[[dv]])
    }

    sformula <- as_formula(dv, between, c(), covar)
    aov <- tryCatch(rstatix::anova_test(dat, sformula, type = type, effect.size = effect.size, detailed = T)
                    , error = function(e) return(NULL))
    if (!is.null(aov)) return(aov)
  })
  if (as.table) toReturn <- get.ancova.table(toReturn)
  return(toReturn)
}

#' Pairwise Comparisons of Estimated Marginal Means for ANCOVA Test
#'
#' This function provides a wrapper for rstatix::emmeans_test for the dependent variables `dvs`
#'
#' @param data a data.frame or list containing the data
#' @param dvs numeric columns with the dependent variables to be used in the anova test
#' @param between independent (between) variable in which perform the anova test
#' @param covar column with the covariate information
#' @param p.adjust.method method to adjust p values for multiple comparisons.
#' @param dv.var column with the information to classify observations
#' @param as.table logical value indicating that the result should be returned after to apply `get.ancova.pwc.table` function
#' @param only.sig logical; if TRUE, only statistical significant results will be tabulated
#' @return A data.frame containing the results for the pairwise comparisons
#' @export
ancova.pwc <- function(data, dvs, between, covar, p.adjust.method = "bonferroni", dv.var = NULL, as.table = F, only.sig = F) {
  ldvs <- as.list(dvs); names(ldvs) <- dvs
  livs <- as.list(as.character(between))
  names(livs) <- as.character(between)
  toReturn <- lapply(ldvs, FUN = function(dv) {
    if (is.data.frame(data)) {
      dat <- as.data.frame(data)
      if (!is.null(dv.var))
        dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
    } else if (is.list(data)) {
      dat <- as.data.frame(data[[dv]])
    }

    lapply(livs, FUN = function(iv) {
      gdat <-  dplyr::group_by_at(dat, dplyr::vars(setdiff(names(livs), iv)))
      emme <- tryCatch(rstatix::emmeans_test(gdat, as.formula(paste0('`',dv,'`'," ~ ",'`',iv,'`')),
                                             covariate = covar, p.adjust.method = p.adjust.method, detailed=T),
                       error = function(e) NULL)
      if (!is.null(emme)) return(emme)
    })
  })
  if (as.table) toReturn <- get.ancova.pwc.table(toReturn, only.sig = only.sig)
  return(toReturn)
}

#' @export
get.ancova.table <- function(aovs) {
  do.call(rbind, lapply(names(aovs), FUN = function(dv) {
    df <- as.data.frame(rstatix::get_anova_table(aovs[[dv]]))
    if (nrow(df) > 0) { cbind(var = dv, rstatix::add_significance(df)) }
  }))
}

#' @export
get.ancova.pwc.table <- function(pwcs, only.sig = F) {
  cnames <- c("var")
  toReturn <- do.call(rbind, lapply(names(pwcs), FUN = function(dv) {
    do.call(rbind, lapply(names(pwcs[[dv]]), FUN = function(iv) {
      cnames <<- c(cnames, iv)
      pwc <- pwcs[[dv]][[iv]]
      pdat <- rstatix::add_significance(pwc)
      if (nrow(pdat) > 0) {
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
  cnames <- unique(c(cnames, colnames(toReturn)[!colnames(toReturn) %in% cnames]))
  return(toReturn[,cnames])
}

#' @export
get.ancova.emmeans <- function(pwcs) {
  cnames <- c("var")
  toReturn <- do.call(rbind, lapply(names(pwcs), FUN = function(dv) {
    do.call(rbind, lapply(names(pwcs[[dv]]), FUN = function(iv) {
      cnames <<- c(cnames, iv)
      pwc <- pwcs[[dv]][[iv]]
      pdat <- rstatix::get_emmeans(pwc)
      if (nrow(pdat) > 0) return(cbind(var = dv, pdat))
    }))
  }))
  toReturn <- toReturn[!duplicated(toReturn[,cnames]),]
  return(toReturn)
}

#' @export
get.ancova.emmeans.with.ds <- function(pwcs, data, dvs, ivs=c(), type = "common", covar = NULL, dv.var = NULL) {
  ds <- get.descriptives(data, dvs, ivs, ifelse(type=="apa-format","common",type), covar, dv.var)

  if (length(covar) > 0)
    ds <- merge(ds[ds$variable!=covar,], ds[ds$variable==covar, !colnames(ds) %in% c('variable')]
                , by=ivs, all.x = T, suffixes = c("", paste0(".",covar)))

  emms <- get.ancova.emmeans(pwcs)
  toReturn <- merge(emms, ds, by.x=c("var",ivs), by.y=c("variable",ivs), suffixes=c(".emms",""))
  toReturn[['sd.emms']] <- sqrt(toReturn[['n']])*toReturn[['se.emms']]

  if (type == "apa-format") {
    toReturn2 <- toReturn[,c("var",ivs,"n",paste0("mean.",covar),paste0("se.",covar),"mean","se","emmean","se.emms")]
    colnames(toReturn2) <- c("var",ivs,"n",paste0(c("M","SE")," (pre)"), paste0(c("M","SE")," (unadj)"), paste0(c("M","SE")," (adj)"))
    toReturn <- toReturn2
  }

  return(toReturn)
}
