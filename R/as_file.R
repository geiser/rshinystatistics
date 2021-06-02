
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

skewness_code <- function(skew, dvname, initTable='dat', dataTable = 'rdat') {
  if (skew %in% c('posSqrt','negSqrt','posLog','negLog','posInv','negInv')) {
    if (skew == 'posSqrt') {
      skewness.code <- paste0('sqrt(',initTable,'[[',dvname,']])')
    } else if (skew == 'negSqrt') {
      skewness.code <- paste0('sqrt(max(',initTable,'[[',dvname,']]+1) - ',initTable,'[[',dvname,']])')
    } else if (skew == 'posLog') {
      skewness.code <- paste0('log10(',initTable,'[[',dvname,']])')
    } else if (skew == 'negLog') {
      skewness.code <- paste0('log10(max(',initTable,'[[',dvname,']]+1) - ',initTable,'[[',dvname,']])')
    } else if (skew == 'posInv') {
      skewness.code <- paste0('1/(',initTable,'[[',dvname,']])')
    } else  if (skew == 'negInv') {
      skewness.code <- paste0('1/(max(',initTable,'[[',dvname,']]+1) - ',initTable,'[[',dvname,']])')
    }
    skewness.code <- paste0(dataTable,'[[',dvname,']] <- ', skewness.code)
    return(skewness.code)
  } else return(NULL)
}
