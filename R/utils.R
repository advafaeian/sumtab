fmt_num_default <- function(x, context){
  if (context$param_name == "p") {
    n = 3
  } else {
    n = 2
  }
  return(format(round(x, n), nsmall=n, trim=T))
}

handle_ps <- function(ps, fmt_num = fmt_num_default){
  p.2 <- ifelse(ps < .0001, 4,
            ifelse(ps < .001, 3,
              ifelse(ps < .01, 2,
                ifelse(ps <.05, 1, 0))))

  ps.next <- fmt_num(ps, list(param_name="p"))

  ps.next <- mapply(function(x,y){
    ifelse(as.numeric(y) == 4, "<0.0001****",
      ifelse(as.numeric(y) == 3, "<0.001***",
        paste0(c(x,rep("*",y)), collapse="")))
    }, ps.next, p.2)

  ps.next  <- ifelse(ps.next =="1.000", "<1", ps.next)
  return(unname(ps.next))
}

# Adapt a user-provided fmt_p_default(x) into a (x, fmt_num) function.
wrap_fmt_p <- function(fmt_p) {

  if (is.null(fmt_p)) return(fmt_p_default)

  f_args <- names(formals(fmt_p))

  if ("fmt_num" %in% f_args) {
    return(function(p, fmt_num) fmt_p(p, fmt_num = fmt_num))
  }

  return(function(p, fmt_num) fmt_p(p))
}


default_fmt_cd <- function(outer, inner) {
  paste0(outer, "±", inner)
}

is_numcat_param <- function(nume, cate, reporting_type="auto"){
  if (reporting_type == "non_parametric") return(FALSE)
  if (reporting_type == "parametric") return(TRUE)
  return(!(shapiro.test(resid(lm(nume ~ cate)))$p.value < 0.05) & !sum(colSums(table(nume, cate)) < 3))
}
is_numnum_param <- function(feature, lmm=NULL, reporting_type="auto"){
  if (reporting_type == "non_parametric") return(FALSE)
  if (reporting_type == "parametric") return(TRUE)
  if (is.null(lmm)) return(!shapiro.test(feature)$p.value < 0.05)
  return(!shapiro.test(rstandard(lmm))$p.value < 0.05)
}

