
#' Factorial ANCOVA test as text
#'
#' @export
factorial.anova.as.text <- function(aovs, data, between, effect.size = "ges") {
  dvs <- names(aovs)
  ivs.str <- paste0(lapply(between, function(iv) {
    vivs <- do.call(c, lapply(dvs, FUN = function(dv) unique(data[[dv]][iv])))
    paste0('"',iv,'" (', paste(as.character(vivs),collapse=', '),')')
  }), collapse = " and ")

  sig.aov.str <- c()
  for (dv in dvs) {
    aov <- aovs[[dv]]
    sig.aov.str <- c(sig.aov.str, sig.aov.as.text(aov, effect.size, dv))
  }
  sig.aov.str <- paste0(sig.aov.str, collapse="\n")

  toReturn <- paste0('An ANOVA between-subjects factor ',ivs.str,
         ' was conducted to determine statistically significant difference on the measured ',
         paste0(paste0('"',dvs,'"'), collapse = ', '),'. ',sig.aov.str,"\n\n")
  return(toReturn)
}

#' Pairwise comparisons as text
#'
#' @export
factorial.anova.pwc.as.text <- function(pwcs, between, p.adjust.method = "bonferroni") {
  emms <- get.anova.emmeans(pwcs)
  pwc.df <- get.anova.pwc.table(pwcs, only.sig = T)
  if (nrow(pwc.df) > 0) {
    pwc.str <- c()
    for (i in seq(1,nrow(pwc.df))) {
      dv <- pwc.df$var[i]
      iv <- between
      if (length(between) > 1)
        iv <- names(pwc.df[i,between])[is.na(pwc.df[i,between])]

      idx <- emms[["var"]] == dv
      lapply(setdiff(between, iv), FUN = function(iv2) {
        idx <<- idx & emms[[iv2]] == pwc.df[i,iv2]
      })

      emm.str <- c()
      for (val in as.character(pwc.df[i, c('group1','group2')])) {
        emm <- emms[idx & emms[[iv]] == val,]
        emm.str <- c(
          emm.str,
          paste0(iv,' = "',val,'" (adj M=',round(emm$emmean[1],4),', SE=',round(emm$se[1],4),')'))
      }
      emm.str <- paste0(emm.str, collapse = ' was significantly different than the ')
      emm.str <- paste0('the least squares means for the ',
                        emm.str, ' with p.adj ', p.val.as.text(pwc.df$p.adj[i]))
      pwc.str <- c(pwc.str, emm.str)
    }
    pwc.str <- paste0('For the measured ',dv,', ', paste0(pwc.str, collapse = ", and "),'.')

    toReturn <- paste0('Pairwise comparisons using Estimated Marginal Means (EMMs) were computed to statistically ',
                       'find significant diferences with p-values ',p.adjust.method,' adjusted. ', pwc.str,"\n\n")
    return(toReturn)
  }
}

