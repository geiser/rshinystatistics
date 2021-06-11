sig.wlx.as.text <- function(wilcoxon.test, ds, iv, lang = 'en') {
  sig.wlx.str <- c()
  sig.wlx <- wilcoxon.test[which(wilcoxon.test[["p"]] <= 0.05),]
  if (nrow(sig.wlx) > 0) {
    for (i in seq(1,nrow(sig.wlx))) {
      dv <- sig.wlx[[".y."]][i]
      cond1 <- sig.wlx[["group1"]][i]
      cond2 <- sig.wlx[["group2"]][i]
      mdn1 <- ds$median[which(ds$variable == dv & ds[[iv]] == cond1)]
      mdn2 <- ds$median[which(ds$variable == dv & ds[[iv]] == cond2)]
      iqr1 <- ds$iqr[which(ds$variable == dv & ds[[iv]] == cond1)]
      iqr2 <- ds$iqr[which(ds$variable == dv & ds[[iv]] == cond2)]

      sig.wlx.str <- c(sig.wlx.str, paste0(
        ifelse(lang=='pt','Para a variável dependente', 'For the dependent variable'),' ',
        '"',dv,'",',' ',
        ifelse(lang=='pt','houve diferença significativa entre','there was a statistically significant difference between'),' ',
        ifelse(lang=='pt','a condição 1','the condition 1'),' ',
        '"',cond1,'" ',
        '(Mdn=',round(mdn1,3),' and IQR=',round(iqr1,3),')',
        ' ',ifelse(lang=='pt','e','and'),' ',
        ifelse(lang=='pt','a condição 2','the condition 2'),' ',
        '"',sig.wlx[["group2"]][i],'" ',
        '(Mdn=',round(mdn2,3),' and IQR=',round(iqr2,3),')',' ',
        ' ',ifelse(lang=='pt','com','with'),' ',
        'W=',round(sig.wlx[["statistic"]][i],2),', ',
        'p ',p.val.as.text(sig.wlx[["p"]][i]), ' ',
        ifelse(lang=='pt', 'e tamanho de efeito de', 'and effect size of'),' ',
        round(sig.wlx[["effsize"]][i],2),' (',sig.wlx[["magnitude"]][i],').'
      ))
    }
    sig.wlx.str <- paste0(sig.wlx.str, collapse = '\n')
  } else {
    if (lang == 'pt')
      sig.wlx.str <- 'Não houve diferenças significativas estatísticas.'
    else
      sig.wlx.str <- 'There was not found a statistically significant difference.'
  }
  return(paste0(sig.wlx.str))
}


#' Text for Pairwise Comparisons of Wilcoxon Signed-Rank Test
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

#' Text for Wilcoxon Signed-Rank Test
#'
#' @export
wilcoxon.as.text <- function(wilcoxon.test, ds, iv, lang = 'en') {
  method <- "Wilcoxon's Mann-Whitney"
  dvs.str <- paste0(lapply(unique(wilcoxon.test[[".y."]]), FUN = function(dv) {
    paste0('"',dv,'"')
  }), collapse = ", ")
  iv1 <- paste0(unique(wilcoxon.test$group1), collapse = ',')
  iv2 <- paste0(unique(wilcoxon.test$group2), collapse = ',')
  txt <- sig.wlx.as.text(wilcoxon.test, ds, iv, lang = lang)
  if (lang == 'pt')
    toReturn <- paste0(method,' testes foram efeituados para comparar as medianas das variáveis dependentes ',
                       dvs.str,' com a variável independente "',iv,'" que apresenta as condições "',iv1,'" (condição 1) e "',iv2,'" (condição 2). ',txt,"\n\n")
  else
    toReturn <- paste0(method,' tests were conducted to compare the medians of dependent variables ',
                       dvs.str,' with the independent variable "',iv,'" that defines the conditions "',iv1,'" (condition 1) and "',iv2,'" (condition 2). ',txt,"\n\n")

  return(toReturn)
}


sig.srh.as.text <- function(srh, sdv = NULL, lang = 'en') {
  lsrh <- list(dv = srh)
  if (!is.null(sdv)) names(lsrh) <- sdv
  srh <- get.scheirer.table(lsrh)
  dof.res <- srh$Df[which(srh$Effect == 'Residuals')]

  sig.srh.str <- c()
  sig.srh <- srh[which(srh$Effect != 'Residuals' & is.numeric(srh[['p.value']]) & srh[['p.value']] < 0.05),]
  if (nrow(sig.srh) > 0) {
    for (i in seq(1,nrow(sig.srh))) {
      eff.str <- paste0(ifelse(lang=='pt','no fator','in the factor'), ' "',sig.srh[["Effect"]][i],'"')
      if (length(strsplit(eff.str,':')[[1]]) > 1) {
        eff.str <- paste0(ifelse(lang=='pt','na interação dos fatores','in the interaction of factors'), ' "',sig.srh[["Effect"]][i],'"')
      }
      sig.srh.str <- c(sig.srh.str, paste0(
        eff.str,' ',
        ifelse(lang=='pt','com','with'),' ',
        'H(',sig.srh[["Df"]][i],',',dof.res,')=',round(sig.srh[["H"]][i],3),
        ifelse(lang=='pt','e','and'),' ',
        'p',p.val.as.text(sig.srh[["p.value"]][i])))
    }
    sig.srh.str <- paste0(
      ifelse(lang=='pt','houve efeitos estatisticamente significativos','there was statistically significant effects'),' ',
      paste0(sig.srh.str, collapse = ifelse(lang=='pt',' e ',' and ')), '.')
  } else {
    if (lang=='pt')
      sig.srh.str <- 'não houve efeitos estatísticamente significativos.'
    else
      sig.srh.str <- 'there was not statistically significant effects.'
  }

  prefix.str <- ''
  if (!is.null(sdv)){
    if (lang=='pt')
      prefix.str  <- paste0('Para a variável dependente "',sdv,'", ')
    else
      prefix.str  <- paste0('For the dependent variable "',sdv,'", ')
  }
  return(paste0(prefix.str, sig.srh.str))
}


sig.kruskal.as.text <- function(kt, ez, sdv = NULL, lang = 'en') {
  kruskal <- merge(kt,ez)
  sig.kruskal.str <- c()
  sig.kruskal <- kt[which(is.numeric(kruskal[['p']]) & kruskal[['p']] < 0.05),]
  if (nrow(sig.kruskal) > 0) {
    for (i in seq(1,nrow(sig.kruskal))) {
      sig.kruskal.str <- c(sig.kruskal.str, paste0(
        ifelse(lang=='pt','com','with'),' ',
        'Chi-square(',sig.kruskal[["df"]][i],')=',round(sig.kruskal[["statistic"]][i],3),
        ', p',p.val.as.text(sig.kruskal[["p"]][i]),' ',
        ifelse(lang=='pt','e tamanho de efeito','and effect size'),' ',
        'eta-square=',round(sig.kruskal$effsize[i],3),' ',
        '(',sig.kruskal$magnitude[i],')'))
    }
    sig.kruskal.str <- paste0(
      ifelse(lang=='pt','houve efeitos estatisticamente significativos','there was statistically significant effects'),' ',
      paste0(sig.kruskal.str, collapse = ifelse(lang=='pt',' e ',' and ')), '.')
  } else {
    if (lang=='pt')
      sig.kruskal.str <- 'não houve efeitos estatísticamente significativos.'
    else
      sig.kruskal.str <- 'there was not statistically significant effects.'
  }

  prefix.str <- ''
  if (!is.null(sdv)){
    if (lang=='pt')
      prefix.str  <- paste0('Para a variável dependente "',sdv,'", ')
    else
      prefix.str  <- paste0('For the dependent variable "',sdv,'", ')
  }
  return(paste0(prefix.str, sig.kruskal.str))
}


#' Text for Kruskal-Wallis Test
#'
#' @export
kruskal.as.text <- function(kruskals, data, between, lang='en') {
  dvs <- names(kruskals)
  ivs.str <- paste0(lapply(between, function(iv) {
    vivs <- unique(unlist(lapply(dvs, FUN = function(dv) {
      vals <- c()
      for (val in unique(data[[dv]][iv]))
        vals <- c(as.character(val), vals)
      return(vals)
    })))
    paste0('"',iv,'" (', paste(vivs,collapse=', '),')')
  }), collapse = ifelse(lang=='pt',' e ', ' and '))

  sig.kruskal.str <- c()
  for (dv in dvs) {
    kruskal <- kruskals[[dv]]
    sig.kruskal.str <- c(sig.kruskal.str, sig.kruskal.as.text(kruskal$kt, kruskal$ez, dv, lang=lang))
  }
  sig.kruskal.str <- paste0(sig.kruskal.str, collapse="\n")

  if (lang=='pt')
    toReturn <- paste0(
      'Testes Kruskal-Wallis com a variável independente entre sujeito ',ivs.str,' ',
      'foram efetuadas para determinar as diferenças significativas estatísticas nas variáveis dependentes ',
      paste0(paste0('"',dvs,'"'), collapse = ', '),'. ',sig.kruskal.str,"\n\n")
  else
    toReturn <- paste0(
      'Kruskal-Wallis tests with independent between-subjects variable ',ivs.str,' ',
      'were performed to determine statistically significant difference on the dependent varibles ',
      paste0(paste0('"',dvs,'"'), collapse = ', '),'. ',sig.kruskal.str,"\n\n")
  return(toReturn)
}


#' Text for Scheirer-Ray-Hare Test
#'
#' @export
srh.as.text <- function(srhs, data, between, lang='en') {
  dvs <- names(srhs)
  ivs.str <- paste0(lapply(between, function(iv) {
    vivs <- unique(unlist(lapply(dvs, FUN = function(dv) {
      vals <- c()
      for (val in unique(data[[dv]][iv]))
        vals <- c(as.character(val), vals)
      return(vals)
    })))

    paste0('"',iv,'" (', paste(vivs,collapse=', '),')')
  }), collapse = ifelse(lang=='pt',' e ', ' and '))

  sig.srh.str <- c()
  for (dv in dvs) {
    srh <- srhs[[dv]]
    sig.srh.str <- c(sig.srh.str, sig.srh.as.text(srh, dv, lang=lang))
  }
  sig.srh.str <- paste0(sig.srh.str, collapse="\n")

  if (lang=='pt')
    toReturn <- paste0(
      'testes Scheirer-Ray-Hare com as variáveis independentes entre sujeitos ',ivs.str,' ',
      'foram efetuadas para determinar as diferenças significativas estatísticas nas variáveis dependentes ',
      paste0(paste0('"',dvs,'"'), collapse = ', '),'. ',sig.srh.str,"\n\n")
  else
    toReturn <- paste0(
      'Scheirer-Ray-Hare tests with independent between-subjects variables ',ivs.str,' ',
      'were performed to determine statistically significant difference on the dependent varibles ',
      paste0(paste0('"',dvs,'"'), collapse = ', '),'. ',sig.srh.str,"\n\n")
  return(toReturn)
}
