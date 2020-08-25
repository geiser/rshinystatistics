
sig.tt.as.text <- function(t.test, hedges.correction = F) {
  sig.tt.str <- c()
  sig.tt <- t.test[which(t.test[["p"]] <= 0.05),]
  if (nrow(sig.tt) > 0) {
    for (i in seq(1,nrow(sig.tt))) {
      sig.tt.str <- c(sig.tt.str, paste0(
        'For the measured ',sig.tt[[".y."]][i],', there was a statistically significant difference in the condition of "',
        sig.tt[["group1"]][i],'" (adj M = ',round(sig.tt[["estimate1"]][i],4),') and "',
        sig.tt[["group2"]][i],'" (adj M = ',round(sig.tt[["estimate2"]][i],4),') with ',
        't(',floor(sig.tt[["df"]][i]),') = ',round(sig.tt[["statistic"]][i],4),', ',
        'p ',p.val.as.text(sig.tt[["p"]][i]),', ',
        ifelse(hedges.correction, "Hedge's g","Cohen's d"),' = ',
        round(sig.tt[["effsize"]][i],4),' (',sig.tt[["magnitude"]][i],').'
      ))
    }
    sig.tt.str <- paste0(sig.tt.str, collapse = '\n')
  } else {
    sig.tt.str <- 'There was not found a statistically significant difference.'
  }
  return(paste0(sig.tt.str))
}

#' T-Test as text
#'
#' @export
ttest.as.text <- function(t.test, iv, var.equal = F, hedges.correction = F) {
  method <- "Welch's"
  if (var.equal) method <- "Student's"
  dvs.str <- paste0(lapply(unique(t.test[[".y."]]), FUN = function(dv) {
    paste0('"',dv,'"')
  }), collapse = ", ")
  iv1 <- paste0(unique(t.test$group1), collapse = ',')
  iv2 <- paste0(unique(t.test$group2), collapse = ',')
  txt <- sig.tt.as.text(t.test, hedges.correction)
  toReturn <- paste0('An independent-samples ',method,' t-test was conducted to compare ',
                     dvs.str,' for "',iv,'" in ',iv1,' and ',iv2,' conditions. ',txt,"\n\n")
  return(toReturn)
}

