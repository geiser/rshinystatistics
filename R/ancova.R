#' Ancova Test
#'
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
  ds <- get.descriptives(data, dvs, ivs, type, covar, dv.var)
  if (length(covar) > 0)
    ds <- merge(ds[ds$variable!=covar,], ds[ds$variable==covar, !colnames(ds) %in% c('variable')]
                , by=ivs, all.x = T, suffixes = c("", paste0(".",covar)))
  emms <- get.ancova.emmeans(pwcs)
  toReturn <- merge(emms, ds, by.x=c("var",ivs), by.y=c("variable",ivs), suffixes=c(".emms",".ds"))
  toReturn[['sd.emms']] <- sqrt(toReturn[['n']])*toReturn[['se.emms']]
  return(toReturn)
}
