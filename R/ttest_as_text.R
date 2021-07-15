sig.tt.as.text <- function(t.test, ds, iv, hedges.correction = F, lang = 'en') {
  sig.tt.str <- c()
  sig.tt <- t.test[which(t.test[["p"]] <= 0.05),]
  if (nrow(sig.tt) > 0) {
    for (i in seq(1,nrow(sig.tt))) {
      dv <- sig.tt[[".y."]][i]
      cond1 <- sig.tt[["group1"]][i]
      cond2 <- sig.tt[["group2"]][i]
      m1 <- ds$mean[which(ds$variable == dv & ds[[iv]] == cond1)]
      m2 <- ds$mean[which(ds$variable == dv & ds[[iv]] == cond2)]
      sd1 <- ds$sd[which(ds$variable == dv & ds[[iv]] == cond1)]
      sd2 <- ds$sd[which(ds$variable == dv & ds[[iv]] == cond2)]

      sig.tt.str <- c(sig.tt.str, paste0(
        ifelse(lang=='pt','Para a variável dependente', 'For the dependent variable'),' ',
        '"',dv,'",',' ',
        ifelse(lang=='pt','houve diferença significativa entre','there was a statistically significant difference between'),' ',
        ifelse(lang=='pt','a condição 1','the condition 1'),' ',
        '"',cond1,'" ',
        '(M=',round(m1,3),' and SD=',round(sd1,3),')',
        ' ',ifelse(lang=='pt','e','and'),' ',
        ifelse(lang=='pt','a condição 2','the condition 2'),' ',
        '"',sig.tt[["group2"]][i],'" ',
        '(M=',round(m2,3),' and SD=',round(sd2,3),')',' ',
        ' ',ifelse(lang=='pt','com','with'),' ',
        't(',round(sig.tt[["df"]][i], 2),') = ',round(sig.tt[["statistic"]][i],2),', ',
        'p ',p.val.as.text(sig.tt[["p"]][i]), ' ',
        ifelse(lang=='pt', 'e tamanho de efeito', 'and effect size'),' ',
        ifelse(hedges.correction, "Hedge's g","Cohen's d"),'=',
        round(sig.tt[["effsize"]][i],2),' (',sig.tt[["magnitude"]][i],').'
      ))
    }
    sig.tt.str <- paste0(sig.tt.str, collapse = '\n')
  } else {
    if (lang == 'pt')
      sig.tt.str <- 'Não houve diferenças significativas estatísticas.'
    else
      sig.tt.str <- 'There was not found a statistically significant difference.'
  }
  return(paste0(sig.tt.str))
}

#' T-Test as textual report using APA style
ttest.as.text <- function(t.test, ds, iv, var.equal = F, hedges.correction = T, lang = 'en') {
  method <- "Welch's"
  if (var.equal) method <- "Student's"
  dvs.str <- paste0(lapply(unique(t.test[[".y."]]), FUN = function(dv) {
    paste0('"',dv,'"')
  }), collapse = ", ")
  iv1 <- paste0(unique(t.test$group1), collapse = ',')
  iv2 <- paste0(unique(t.test$group2), collapse = ',')
  txt <- sig.tt.as.text(t.test, ds, iv, hedges.correction, lang = lang)
  if (lang == 'pt')
    toReturn <- paste0(method,' testes t de amostras independentes foram conduzido para comparar as medias das variáveis dependentes ',
                       dvs.str,' com a variável independente "',iv,'" que apresenta as condições "',iv1,'" (condição 1) e "',iv2,'" (condição 2). ',txt,"\n\n")
  else
    toReturn <- paste0('Independent-samples ',method,' t-tests were conducted to compare the means of dependent variables ',
                       dvs.str,' with the independent variable "',iv,'" that defines the conditions "',iv1,'" (condition 1) and "',iv2,'" (condition 2). ',txt,"\n\n")

  return(toReturn)
}

