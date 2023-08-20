#' ANOVA Test
#'
#' This function provides a wrapper for rstatix::anova_test for the dependent variables `dvs`
#' including their effect sizes
#'
#' @param data a data.frame or list containing the data
#' @param dvs numeric columns with the dependent variables to be used in the anova test
#' @param between independent (between) variable in which perform the anova test
#' @param within independent (within) variable in which perform the anova test
#' @param wid column with the unique identification
#' @param type the type of sums of squares for ANOVA. Allowed values are either 1, 2 or 3. Default value is 2.
#' @param effect.size the effect size to be computed, being "ges" (generalized eta squared) or "pes" (partial eta squared) Default is "ges".
#' @param dv.var column with the information to classify observations
#' @param as.table logical value indicating that the result should be returned after to apply `get.anova.table` function
#' @param skewness named list in which each name represent the column of skewness data
#' @return A data.frame containing the results for the anova test and their effect sizes
#' @export
anova.test <- function(data, dvs, between=c(), within=c(), wid = 'row.pos', type = NULL
                       , effect.size = 'ges', dv.var = NULL, as.table = F, skewness = c()) {
  ldvs <- as.list(dvs); names(ldvs) <- dvs
  toReturn <- lapply(ldvs, FUN = function(dv) {
    if (is.data.frame(data)) {
      dat <- as.data.frame(data)
      if (!is.null(dv.var))
        dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
    } else if (is.list(data)) {
      dat <- as.data.frame(data[[dv]])
    }

    for (col in names(skewness))
      dat[[col]] <- dat[[skewness[[col]]]]

    rbetween <- intersect(as.character(between),colnames(dat))
    rwithin <- intersect(as.character(within),colnames(dat))

    sformula <- as_formula(dv, rbetween, rwithin, wid = wid)
    aov <- tryCatch(rstatix::anova_test(dat, sformula, type = type, effect.size = effect.size, detailed = T)
                    , error = function(e) return(NULL))
    if (!is.null(aov)) return(aov)
  })
  if (as.table) toReturn <- get.anova.table(toReturn)
  return(toReturn)
}

#' Pair-wise Anova Test
#'
#' @export
anova.pwc <- function(data, dvs, between=c(), within=c(), p.adjust.method = "bonferroni", dv.var = NULL, as.table = F, only.sig = F, skewness = c()) {
  ldvs <- as.list(dvs); names(ldvs) <- dvs

  toReturn <- lapply(ldvs, FUN = function(dv) {
    if (is.data.frame(data)) {
      dat <- as.data.frame(data)
      if (!is.null(dv.var))
        dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
    } else if (is.list(data)) {
      dat <- as.data.frame(data[[dv]])
    }

    rbetween <- intersect(as.character(between),colnames(dat))
    rwithin <- intersect(as.character(within),colnames(dat))

    livs <- as.list(c(rbetween, rwithin))
    names(livs) <- c(rbetween, rwithin)

    lapply(livs, FUN = function(iv) {
      gdat <- dat[,c(names(livs),dv)]
      for (f in c(names(livs))) {
        if (is.factor(gdat[[f]])) {
          levs = levels(gdat[[f]])
          gdat[[f]] <- factor(gdat[[f]], levs[levs %in% unique(gdat[[f]])])
        }
      }
      gdat <- dplyr::group_by_at(gdat, dplyr::vars(setdiff(names(livs), iv)))

      col <- dv; if (col %in% names(skewness)) col <- skewness[[dv]]
      if (length(rwithin) > 0) {
        pwc <- tryCatch(rstatix::pairwise_t_test(gdat, as.formula(paste0('`',dv,'`'," ~ ",'`',iv,'`')),
                                                 paired = iv %in% rwithin,
                                                 p.adjust.method = p.adjust.method, detailed=T)
                        , error = function(e) NULL)
        if (col != dv) {
          pwc2 <- rstatix::pairwise_t_test(gdat, as.formula(paste0('`',col,'`'," ~ ",'`',iv,'`')),
                                           paired = iv %in% rwithin,
                                           p.adjust.method = p.adjust.method, detailed=T)
          pwc[[".y."]] <- pwc2[[".y."]]
          pwc[["statistic"]] <- pwc2[["statistic"]]
          pwc[["df"]] <- pwc2[["df"]]
          pwc[["p"]] <- pwc2[["p"]]
          pwc[["p.adj"]] <- pwc2[["p.adj"]]
          pwc[["p.signif"]] <- pwc2[["p.signif"]]
          pwc[["p.adj.signif"]] <- pwc2[["p.adj.signif"]]
        }

      } else {
        pwc <- tryCatch(rstatix::emmeans_test(gdat, as.formula(paste0('`',dv,'`'," ~ ",'`',iv,'`')),
                                              p.adjust.method = p.adjust.method, detailed=T)
                        , error = function(e) NULL)
        if (col != dv) {
          pwc2 <- rstatix::emmeans_test(gdat, as.formula(paste0('`',col,'`'," ~ ",'`',iv,'`')),
                                        p.adjust.method = p.adjust.method, detailed=T)
          pwc[[".y."]] <- pwc2[[".y."]]
          pwc[["statistic"]] <- pwc2[["statistic"]]
          pwc[["df"]] <- pwc2[["df"]]
          pwc[["p"]] <- pwc2[["p"]]
          pwc[["p.adj"]] <- pwc2[["p.adj"]]
          pwc[["p.signif"]] <- pwc2[["p.signif"]]
          pwc[["p.adj.signif"]] <- pwc2[["p.adj.signif"]]
        }
      }

      if (!is.null(pwc)) return(pwc)
    })
  })
  if (as.table) toReturn <- get.anova.pwc.table(toReturn, only.sig = only.sig)
  return(toReturn)
}

#' @export
get.anova.table <- function(aovs) {
  do.call(rbind, lapply(names(aovs), FUN = function(dv) {
    df <- as.data.frame(rstatix::get_anova_table(aovs[[dv]]))
    if (nrow(df) > 0) { cbind(var = dv, rstatix::add_significance(df)) }
  }))
}

#' @export
get.anova.pwc.table <- function(pwcs, only.sig = F) {

  cnames <- unique(do.call(c, lapply(pwcs, FUN = function(pwc2) {
    do.call(c, lapply(pwc2, FUN = function(pwc) colnames(pwc)))
  })))

  toReturn <- do.call(rbind, lapply(names(pwcs), FUN = function(dv) {
    do.call(rbind, lapply(names(pwcs[[dv]]), FUN = function(iv) {
      pwc <- pwcs[[dv]][[iv]]
      if (!is.null(pwc)) {
        pdat <- rstatix::add_significance(pwc)
        if (nrow(pdat) > 0) {
          for (cnam in setdiff(cnames, colnames(pdat))) {
            pdat[[cnam]] <- NA
          }
          return(cbind(var = dv, pdat))
        }
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
  cnames <- c('method','alternative',
              '.y.','group1','group2','n1','n2','estimate',
              'df','statistic','conf.low','conf.high','p','p.adj','p.signif','p.adj.signif')
  cnames <- intersect(cnames, colnames(toReturn))
  cnames <- c('var',setdiff(colnames(toReturn), cnames),cnames)
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
  ds <- get.descriptives(data, dvs, ivs, ifelse(type=="apa-format","common",type), dv.var)
  emms <- get.anova.emmeans(pwcs)
  toReturn <- merge(emms, ds, by.x=c("var",ivs), by.y=c("variable",ivs), suffixe=c(".emms",""))
  toReturn[['sd.emms']] <- sqrt(toReturn[['n']])*toReturn[['se.emms']]

  if (type == "apa-format") {
    toReturn2 <- toReturn[,c("var",ivs,"n","mean","se")]
    colnames(toReturn2) <- c("var",ivs,"n", "M","SE")
    toReturn <- toReturn2
  }

  return(toReturn)
}


