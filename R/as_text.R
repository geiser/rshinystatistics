
p.val.as.text <- function(p.val) {
  if (p.val < 0.001) return("<0.001") else return(paste0('=',round(p.val,3)))
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


sig.aov.as.text <- function(aov, effect.size = "ges", sdv = NULL, lang = 'en') {
  sig.aov.str <- c()
  sig.aov <- aov[which(is.numeric(aov[['p']]) & aov[['p']] < 0.05),]
  if (nrow(sig.aov) > 0) {
    for (i in seq(1,nrow(sig.aov))) {
      eff.str <- paste0(ifelse(lang=='pt','no fator','in the factor'), ' "',sig.aov[["Effect"]][i],'"')
      if (length(strsplit(eff.str,':')[[1]]) > 1) {
        eff.str <- paste0(ifelse(lang=='pt','na interação dos fatores','in the interaction of factors'), ' "',sig.aov[["Effect"]][i],'"')
      }
      sig.aov.str <- c(sig.aov.str, paste0(
        eff.str,' ',
        ifelse(lang=='pt','com','with'),' ',
        'F(',sig.aov[["DFn"]][i],',',sig.aov[["DFd"]][i],')=',round(sig.aov[["F"]][i],3),
        ', p',p.val.as.text(sig.aov[["p"]][i]),' ',
        ifelse(lang=='pt','e','and'),' ',
        effect.size,'=',round(sig.aov[[effect.size]][i],3),' ',
        ifelse(lang=='pt','(tamanho de efeito)','(effect size)')))
    }
    sig.aov.str <- paste0(
      ifelse(lang=='pt','houve efeitos estatisticamente significativos','there was statistically significant effects'),' ',
      paste0(sig.aov.str, collapse = ifelse(lang=='pt',' e ',' and ')), '.')
  } else {
    if (lang=='pt')
      sig.aov.str <- 'não houve efeitos estatísticamente significativos.'
    else
      sig.aov.str <- 'there was not statistically significant effects.'
  }

  prefix.str <- ''
  if (!is.null(sdv)){
    if (lang=='pt')
      prefix.str  <- paste0('Para a variável dependente "',sdv,'", ')
    else
      prefix.str  <- paste0('For the dependent variable "',sdv,'", ')
  }
  return(paste0(prefix.str, sig.aov.str))
}

#' Pairwise comparisons from ANCOVA or ANOVA as text
aov.pwc.as.text <- function(test, pwcs, ds, between, p.adjust.method = "bonferroni", lang='en') {

  if (test == 'ancova') {
    emms <- get.ancova.emmeans(pwcs)
    pwc.df <- get.ancova.pwc.table(pwcs, only.sig = T)
  } else if (test == 'anova') {
    emms <- get.anova.emmeans(pwcs)
    pwc.df <- get.anova.pwc.table(pwcs, only.sig = T)
  }

  if (nrow(pwc.df) > 0) {
    pwc.str <- c()
    for (i in seq(1,nrow(pwc.df))) {
      dv <- pwc.df$var[i]
      iv <- between
      if (length(between) > 1)
        iv <- names(pwc.df[i,between])[is.na(pwc.df[i,between])]

      idx <- (ds[["var"]] == dv)
      lapply(setdiff(between, iv), FUN = function(iv2) {
        idx <<- idx & ds[[iv2]] == pwc.df[[iv2]][i]
      })

      emm.str <- c()
      for (val in as.character(pwc.df[i, c('group1','group2')])) {
        val <- as.character(val)
        m0 <- round(ds$emmean[which(idx & ds[[iv]] == val)],3)
        sd0 <- round(ds[['sd']][which(idx & ds[[iv]] == val)],3)
        emm.str <- c(
          emm.str,
          paste0(ifelse(lang=='pt','a média no','the mean in the'),' ',
                 iv,'="',val,'" (adj M=',m0,' ',ifelse(lang=='pt','e','and'),' SD=',sd0,')'))
      }

      emm.str <- paste0(emm.str,
                        collapse = ifelse(lang=='pt',
                                          ' foi significativamente diferente do que ',
                                          ' was significantly different than '))
      emm.str <- paste0(emm.str, ' ',ifelse(lang=='pt','com','with'),' p-adj',p.val.as.text(pwc.df$p.adj[i]))
      pwc.str <- c(pwc.str, emm.str)
    }
    pwc.str <- paste0(ifelse(lang=='pt','Para a variável dependente','For the dependent variable'),' ',
                      '"',dv,'", ', paste0(pwc.str, collapse='; '),'.')

    if(lang=='pt')
      toReturn <- paste0(
        'Comparações emparelhadas usando o Estimated Marginal Means (EMMs) ',
        'foram computadas para encontrar diferenças estatítiscamente significativas entre os grupos ',
        'definidos pelas variáveis independentes e com os p-values ajustado pelo método ',
        '"',p.adjust.method,'". ', pwc.str,"\n\n")
    else
      toReturn <- paste0(
        'Pairwise comparisons using the Estimated Marginal Means (EMMs) ',
        'were computed to find statistically significant diferences among the groups ',
        'defined by the independent variables, and with the p-values ajusted by the method ',
        '"',p.adjust.method,'". ', pwc.str,"\n\n")

    return(toReturn)
  }
}
