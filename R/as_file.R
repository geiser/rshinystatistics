
list.as.code <- function(vlist) {
  code.out <- paste0(unlist(lapply(names(vlist), FUN = function(rvar) {
    ids <- as.character(vlist[[rvar]])
    if (length(ids) > 0){
      code.line <- paste0('"',rvar,'"'," = c(", paste0(paste0('"',ids,'"'), collapse=','),")")
      return(code.line)
    } else return(NULL)
  })), collapse = ",\n")
  paste0("list(\n", code.out, "\n)")
}

skewness_code <- function(dataname, skew, dvname) {
  if (skew %in% c('posSqrt','negSqrt','posLog','negLog','posInv','negInv')) {
    if (skew == 'posSqrt') {
      skewness.code <- paste0('sqrt(',dataname,'[[',dvname,']])')
    } else if (skew == 'negSqrt') {
      skewness.code <- paste0('sqrt(max(',dataname,'[[',dvname,']]+1) - ',dataname,'[[',dvname,']])')
    } else if (skew == 'posLog') {
      skewness.code <- paste0('log10(',dataname,'[[',dvname,']])')
    } else if (skew == 'negLog') {
      skewness.code <- paste0('log10(max(',dataname,'[[',dvname,']]+1) - ',dataname,'[[',dvname,']])')
    } else if (skew == 'posInv') {
      skewness.code <- paste0('1/(',dataname,'[[',dvname,']])')
    } else  if (skew == 'negInv') {
      skewness.code <- paste0('1/(max(',dataname,'[[',dvname,']]+1) - ',dataname,'[[',dvname,']])')
    }
    skewness.code <- paste0(dataname,'[[',dvname,']] <- ', skewness.code)
    return(skewness.code)
  } else return(NULL)
}
