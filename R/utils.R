rof <- function(x, n=2){
  return(format(round(x, n), nsmall=n, trim=T))
}

handle_ps <- function(ps, round.n=3){
  p.2 <- ifelse(ps < .0001, 4,
            ifelse(ps < .001, 3,
              ifelse(ps < .01, 2,
                ifelse(ps <.05, 1, 0))))

  ps.next <- rof(ps, round.n)

  ps.next <- mapply(function(x,y){
    ifelse(as.numeric(y) == 4, "<0.0001****",
      ifelse(as.numeric(y) == 3, "<0.001***",
        paste0(c(x,rep("*",y)), collapse="")))
    }, ps.next, p.2)

  ps.next  <- ifelse(ps.next =="1.000", "<1", ps.next)
  return(unname(ps.next))
}


is_numcat_param <- function(nume, cate, reporting_type){
  if (reporting_type == "non_parametric") return(FALSE)
  if (reporting_type == "parametric") return(TRUE)
  return(!(shapiro.test(resid(lm(nume ~ cate)))$p.value < 0.05) & !sum(colSums(table(nume, cate)) < 2))
}
is_numnum_param <- function(feature, lmm=NULL, reporting_type){
  if (reporting_type == "non_parametric") return(FALSE)
  if (reporting_type == "parametric") return(TRUE)
  if (is.null(lmm)) return(!shapiro.test(feature)$p.value < 0.05)
  return(!shapiro.test(rstandard(lmm))$p.value < 0.05)
}

