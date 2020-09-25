
#' Factorial ANCOVA test as text
#'
#' @export
factorial.anova.as.text <- function(aovs, data, between, effect.size = "ges", lang='en') {
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
      'testes ANOVA com as variáveis independentes entre sujeitos ',ivs.str,' ',
      'foram efetuadas para determinar as diferenças significativas estatísticas nas variáveis dependentes ',
      paste0(paste0('"',dvs,'"'), collapse = ', '),'. ',sig.aov.str,"\n\n")
  else
    toReturn <- paste0(
      'ANOVA tests with independent between-subjects variables ',ivs.str,' ',
      'were performed to determine statistically significant difference on the dependent varibles ',
      paste0(paste0('"',dvs,'"'), collapse = ', '),'. ',sig.aov.str,"\n\n")
  return(toReturn)
}


