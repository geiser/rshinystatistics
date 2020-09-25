
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


#' Scheirer-Ray-Hare test as text
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
