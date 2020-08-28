

#' @export
get.anova.table <- function(aovs) {
  do.call(rbind, lapply(names(aovs), FUN = function(dv) {
    df <- as.data.frame(rstatix::get_anova_table(aovs[[dv]]))
    if (nrow(df) > 0) { cbind(var = dv, rstatix::add_significance(df)) }
  }))
}


#' @export
get.anova.test <- function(data, dvs, between=c(), within=c(), type, effect.size, dv.var = NULL, as.table = F) {
  ldvs <- as.list(dvs); names(ldvs) <- dvs
  toReturn <- lapply(ldvs, FUN = function(dv) {
    dat <- as.data.frame(data)
    if (!is.null(dv.var))
      dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
    sformula <- as_formula(dv, between, within)
    aov <- tryCatch(rstatix::anova_test(dat, sformula, type = type, effect.size = effect.size, detailed = T)
                    , error = function(e) return(NULL))
    if (!is.null(aov)) return(aov)
  })
  if (as.table) toReturn <- get.anova.table(toReturn)
  return(toReturn)
}


#' @export
get.anova.pwc.table <- function(pwcs, only.sig = F) {
  print(pwcs)
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
get.anova.emmeans <- function(pwcs) {
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
get.anova.emmeans.with.ds <- function(pwcs, data, dvs, ivs=c(), type = "common", dv.var = NULL) {
  ds <- descriptive_statistics(data, dvs, ivs, type, dv.var)
  emms <- get.anova.emmeans(pwcs)
  merge(emms, ds, by.x=c("var",ivs), by.y=c("variable",ivs), suffixes=c(".emms",".ds"))
}

#' @export
get.anova.pwc <- function(data, dvs, between=c(), within=c(), p.adjust.method = "bonferroni", dv.var = NULL, as.table = F, only.sig = F) {
  ldvs <- as.list(dvs); names(ldvs) <- dvs
  livs <- as.list(as.character(c(between, within)))
  names(livs) <- as.character(c(between, within))
  toReturn <- lapply(ldvs, FUN = function(dv) {
    dat <- as.data.frame(data)
    if (!is.null(dv.var))
      dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
    lapply(livs, FUN = function(iv) {
      gdat <-  dplyr::group_by_at(dat, dplyr::vars(setdiff(names(livs), iv)))
      emme <- tryCatch(rstatix::emmeans_test(gdat, as.formula(paste0('`',dv,'`'," ~ ",'`',iv,'`')), p.adjust.method = p.adjust.method, detailed=T)
                       , error = function(e) NULL)
      if (!is.null(emme)) return(emme)
    })
  })
  if (as.table) toReturn <- get.anova.pwc.table(toReturn, only.sig = only.sig)
  return(toReturn)
}

