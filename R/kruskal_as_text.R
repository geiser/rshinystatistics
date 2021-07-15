
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
