#' ANCOVA test as text
ancova.as.text <- function(aovs, data, between, covar, effect.size = "ges", lang='en') {
  dvs <- names(aovs)
  ivs.str <- paste0(lapply(between, function(iv) {
    vivs <- unique(unlist(lapply(dvs, FUN = function(dv) {
      vals <- c()
      for (val in unique(data[[dv]][iv]))
        vals <- c(as.character(val), vals)
      return(vals)
    })))

    paste0('"',iv,'" (', paste(vivs,collapse=', '),')')
  }), collapse = ifelse(lang=='pt',' e ', ' and '))

  sig.aov.str <- c()
  for (dv in dvs) {
    aov <- aovs[[dv]]
    sig.aov.str <- c(sig.aov.str, sig.aov.as.text(aov, effect.size, dv, lang=lang))
  }
  sig.aov.str <- paste0(sig.aov.str, collapse="\n")

  if (lang=='pt')
    toReturn <- paste0(
      'Depois de controlar a linearidade da covariância "',covar,'", ',
      'testes ANCOVA com as variáveis independentes entre sujeitos ',ivs.str,' ',
      'foram efetuadas para determinar as diferenças significativas estatísticas nas variáveis dependentes ',
      paste0(paste0('"',dvs,'"'), collapse = ', '),'. ',sig.aov.str,"\n\n")
  else
    toReturn <- paste0(
      'After controlling the linearity of covariance "',covar,'", ',
      'ANCOVA tests with independent between-subjects variables ',ivs.str,' ',
      'were performed to determine statistically significant difference on the dependent varibles ',
      paste0(paste0('"',dvs,'"'), collapse = ', '),'. ',sig.aov.str,"\n\n")
  return(toReturn)
}
