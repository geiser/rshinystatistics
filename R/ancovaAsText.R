p.val.as.text <- function(p.val) {
  if (p.val < 0.0001) return("< 0.0001") else return(paste0('= ',round(p.val,4)))
}


non.aov.as.text <- function(aov, effect.size = "ges", sdv = NULL) {
  non.aov.str <- c()
  non.aov <- aov[which(is.na(aov[['p<.05']]) | aov[['p<.05']] == ""),]
  if (nrow(non.aov) > 0) {
    for (i in seq(1,nrow(non.aov))) {
      eff.str <- non.aov[["Effect"]][i]
      if (length(strsplit(eff.str,':')[[1]]) > 1) {
        eff.str <- paste0('no interaction between ', paste0(strsplit(eff.str,':')[[1]], collapse = ' and '))
      }
      non.aov.str <- c(non.aov.str, paste0(
        eff.str,' with F(',non.aov[["DFn"]][i],',',non.aov[["DFd"]][i],') = ',round(non.aov[["F"]][i],4),
        ', p ',p.val.as.text(non.aov[["p"]][i]),', and ',effect.size,' = ',round(non.aov[[effect.size]][i],4),' (effect size)'))
    }
  } else {
    return(" ")
  }
  non.aov.str <- paste0(paste0(non.aov.str, collapse="; or "),'.')

  prefix.str <- 'The results revealed no effects of '
  if (!is.null(sdv))
    prefix.str  <- paste0('For the measured ',sdv,', the results revealed no effects of ')
  return(paste0(prefix.str, non.aov.str))
}


sig.aov.as.text <- function(aov, effect.size = "ges", sdv = NULL) {
  sig.aov.str <- c()
  sig.aov <- aov[which(!is.na(aov[['p<.05']]) & aov[['p<.05']] != ""),]
  if (nrow(sig.aov) > 0) {
    for (i in seq(1,nrow(sig.aov))) {
      eff.str <- sig.aov[["Effect"]][i]
      if (length(strsplit(eff.str,':')[[1]]) > 1) {
        eff.str <- paste0('interaction between ', paste0(strsplit(eff.str,':')[[1]], collapse = ' and '))
      }
      sig.aov.str <- c(sig.aov.str, paste0(
        'the ',eff.str,' with F(',sig.aov[["DFn"]][i],',',sig.aov[["DFd"]][i],') = ',round(sig.aov[["F"]][i],4),
        ', p ',p.val.as.text(sig.aov[["p"]][i]),', and ',effect.size,' = ',round(sig.aov[[effect.size]][i],4),' (effect size)'))
    }
    sig.aov.str <- paste0('was statistically significant effects of ', paste0(sig.aov.str, collapse = "; and "), '.')
  } else {
    sig.aov.str <- 'was not statistically significant effects.'
  }

  prefix.str <- 'There '
  if (!is.null(sdv))
    prefix.str  <- paste0('For the measured ',sdv,', there ')

  return(paste0(prefix.str, sig.aov.str))
}

#' ANCOVA test as text
#'
#' @export
ancova.as.text <- function(aovs, data, between, covar, effect.size = "ges") {
  ivs.str <- paste0(lapply(between, function(iv)
    paste0('"',iv,'" (', paste(as.character(unique(data[[iv]])),collapse=', '),')')
  ), collapse = " and ")
  dvs <- names(aovs)

  sig.aov.str <- c()
  for (dv in dvs) {
    aov <- aovs[[dv]]
    sig.aov.str <- c(sig.aov.str, sig.aov.as.text(aov, effect.size, dv))
  }
  sig.aov.str <- paste0(sig.aov.str, collapse="\n")

  toReturn <- paste0('After controlling the "',covar,'", an ANCOVA between-subjects factor ',ivs.str,
         ' was conducted to determine statistically significant difference on the measured ',
         paste0(paste0('"',dvs,'"'), collapse = ', '),'. ',sig.aov.str,"\n\n")
  return(toReturn)
}

#' Pairwise comparisons as text
#'
#' @export
ancova.pwc.as.text <- function(pwcs, between, p.adjust.method = "bonferroni") {
  emms <- get.anvoca.emmeans(pwcs)
  pwc.df <- get.ancova.pwc.table(pwcs, only.sig = T)
  if (nrow(pwc.df) > 0) {
    pwc.str <- c()
    for (i in seq(1,nrow(pwc.df))) {
      dv <- pwc.df$var[i]
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

