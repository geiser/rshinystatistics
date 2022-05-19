#' Scheirer Ray Hare test
#'
#' This function provides a wrapper for rcompanion::scheirerRayHare for the dependent variables `dvs`
#' including their effect sizes
#'
#' @param data a data.frame or list containing the data
#' @param dvs numeric columns with the dependent variables to be used in the scheirer test
#' @param between independent (between) variable in which perform the scheirer test
#' @param dv.var column with the information to classify observations
#' @param as.table logical value indicating that the result should be returned as table or as list
#' @return A data.frame containing the results for the scheirer test and their effect sizes
#' @export
scheirer.test <- function(data, dvs, between, dv.var = NULL, as.table = F) {
  ldvs <- as.list(dvs); names(ldvs) <- dvs
  toReturn <- lapply(ldvs, FUN = function(dv) {
    if (is.data.frame(data)) {
      dat <- as.data.frame(data)
      if (!is.null(dv.var))
        dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
    } else if (is.list(data)) {
      dat <- as.data.frame(data[[dv]])
    }

    sformula <- as_formula(dv, between)
    srh <- tryCatch(rcompanion::scheirerRayHare(sformula, data = dat, verbose = F)
                    , error = function(e) return(NULL))
    if (!is.null(srh)) {
      return(srh)
    }
  })
  if (as.table) toReturn <- get.scheirer.table(toReturn)
  return(toReturn)
}

#' @export
scheirer.pwc <- function(data, dvs, between, pwc.method = "wilcoxon", p.adjust.method = "bonferroni", dv.var = NULL, as.table = F, only.sig = F) {
  ldvs <- as.list(dvs); names(ldvs) <- dvs
  livs <- as.list(as.character(c(between)))
  names(livs) <- as.character(c(between))
  toReturn <- lapply(ldvs, FUN = function(dv) {
    if (is.data.frame(data)) {
      dat <- as.data.frame(data)
      if (!is.null(dv.var))
        dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
    } else if (is.list(data)) {
      dat <- as.data.frame(data[[dv]])
    }

    lapply(livs, FUN = function(iv) {
      pwc <- NULL
      gdat <-  dplyr::group_by_at(dat, dplyr::vars(setdiff(names(livs), iv)))
      if (pwc.method == "wilcoxon") {
        pwc <- tryCatch(
          rstatix::pairwise_wilcox_test(gdat, as.formula(paste0('`',dv,'` ~ `',iv,'`')),
                                        p.adjust.method=p.adjust.method, detailed=T),
          error = function(e) NULL)
      } else {
        pwc <- tryCatch(
          rstatix::dunn_test(gdat, as.formula(paste0('`',dv,'` ~ `',iv,'`')),
                             p.adjust.method=p.adjust.method, detailed=T),
          error = function(e) NULL)
      }
      if (!is.null(pwc)) return(pwc)
    })
  })
  if (as.table) toReturn <- get.scheirer.pwc.table(toReturn, only.sig = only.sig)
  return(toReturn)
}

#' @export
get.scheirer.table <- function(srhs) {
  do.call(rbind, lapply(names(srhs), FUN = function(dv) {
    df <- as.data.frame(srhs[[dv]])
    df <- as.data.frame(cbind("Effect" = rownames(df), df))
    if (nrow(df) > 0) {
      rownames(df) <- seq(1, nrow(df))
      cbind(var = dv, rstatix::add_significance(df))
    }
  }))
}

#' @export
get.scheirer.pwc.table <- function(pwcs, only.sig = F) {
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
