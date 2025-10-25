short_columns = c("Variable","Feature Levels","Total")

long_columns = c("Variable","Feature Levels","Total","Control", "Case", "Estimate","95% CI","P")

# returns expected row names
expected_rows <- function(complete=T){
  exp_r <- sapply(names(mock_data), function(x){

    lvls <- levels(mock_data[[x]])
    if (!is.null(lvls) && (complete || length(lvls)>2)) {
      return(c(x, rep("", length(lvls) - 1)))
    }
    return(x)

  })

  return(unlist(exp_r))
}

cell_extactor <- function(s){
  out <- regmatches(s, gregexpr("-?[0-9]+(\\.[0-9]+)?|Inf", s))
  out <- as.numeric(unlist(out))
  return(out)
}


get_value <- function(row, column, data, offset=0){
  r <- which(data[,"Variable"] == row)
  r = r + as.numeric(offset) #if offset is passed as a string
  val <- data[r, column]
  return(cell_extactor(val))
}


test_descriptive_num <- function(fea, by, reporting_type, result, tolerance = .01) {

  fea_levels <- levels(mock_data[[fea]])
  by_levels <- levels(mock_data[[by]])

  funcs <- list()

  if (reporting_type == "parametric") funcs <- list(central= mean, dispersion= sd)
  else if (reporting_type == "non_parametric") funcs <- list(central= median, dispersion= IQR)

  if (is.null(fea_levels) & is.null(by_levels)){ # num num
    values <- get_value(fea, by, result)

    expect_length(values, 0)
    # expect_equal(values[1], funcs$central(mock_data[[by]], na.rm=TRUE), tolerance = tolerance)
    # expect_equal(values[2], funcs$dispersion(mock_data[[by]], na.rm=TRUE), tolerance = tolerance)

  } else if (!is.null(by_levels)) { # num cate
    # total
    values <- get_value(fea, "Total", result)
    expect_equal(values[1], funcs$central(mock_data[[fea]], na.rm=TRUE), tolerance = tolerance)
    expect_equal(values[2], funcs$dispersion(mock_data[[fea]], na.rm=TRUE), tolerance = tolerance)

    for (lvl in by_levels){
      values <- get_value(fea, lvl, result)
      expect_equal(values[1], funcs$central(mock_data[mock_data[[by]] == lvl, fea] , na.rm=TRUE), tolerance = tolerance)
      expect_equal(values[2], funcs$dispersion(mock_data[mock_data[[by]] == lvl, fea], na.rm=TRUE), tolerance = tolerance)
    }
  } else if (!is.null(fea_levels)){ # cate num
    for (lvl in seq_along(fea_levels)){
      values <- get_value(fea, by, result, offset = lvl - 1)

      expect_equal(values[1], funcs$central(mock_data[mock_data[[fea]] == fea_levels[lvl], by] , na.rm=TRUE), tolerance = tolerance)
      expect_equal(values[2], funcs$dispersion(mock_data[mock_data[[fea]] == fea_levels[lvl], by], na.rm=TRUE), tolerance = tolerance)
    }
  }
}



test_descriptive_catecate <- function(fea, by, tolerance = .01) {

  fea_levels <- levels(fea)
  by_levels <- levels(by)


  for (i in seq_along(fea_levels)){

    # Total
    values <- get_value(fea, "Total", result, offset = i- 1)
    expect_equal(values[1], table(mock_data[[fea]])[[fea_levels[i]]], tolerance = tolerance)
    expect_equal(values[2], prop.table(table(mock_data[[fea]]))[[fea_levels[i]]] * 100, tolerance = tolerance)

    # groups
    exp_table <- table(mock_data[[fea]], mock_data[[by]])
    exp_ptable <- prop.table(exp_table, 2)

    for (lvl in by_levels){
      values <- get_value(by, lvl, result)
      expect_equal(values[1], exp_table['-', lvl],  tolerance = tolerance)
      expect_equal(values[2], exp_ptable['-', lvl] * 100, tolerance = tolerance)
    }
  }
}

test_inferential <- function(fea.name, by.name, test.name, result, tolerance = .01){
  fea <- mock_data[[fea.name]]
  by <- mock_data[[by.name]]

  test <- list()
  if (test.name == 't') {
    if (is.numeric(fea)){
      test <- t.test(fea ~ by);
      test$est <- diff(tapply(fea, by, mean, na.rm = T))
    }
    else if (is.numeric(by)){
      test <- t.test(by ~ fea)
      test$est <- diff(tapply(by, fea, mean, na.rm = T))
    }
  }
  else if (test.name == 'wilcox') {
    if (is.numeric(fea)){
      test <- wilcox.test(fea ~ by, conf.int = TRUE);
    }
    else if (is.numeric(by)){
      test <- wilcox.test(by ~ fea, conf.int = TRUE)
    }
    test$est <- test$estimate
  }
  else if (test.name == 'chis') {
    test <- chisq.test(fea, by)
  }
  else if (test.name == 'fisher') {
    test <- fisher.test(fea, by, conf.int = T)
  }
  else if (test.name == 'aov') {
    if (is.numeric(fea)) test$p.value <- summary(aov(fea ~ by))[[1]][["Pr(>F)"]][1]
    else if (is.numeric(by)) test$p.value <- summary(aov(by ~ fea))[[1]][["Pr(>F)"]][1]
  }
  else if (test.name == "kruksal") {
    if (is.numeric(fea)) test <- kruskal.test(fea ~ by)
    else if (is.numeric(by)) test <- kruskal.test(by ~ fea)
  }
  else if (test.name == "pearsonr") {
    lmm <- lm(by ~ fea)
    test$p.value <- cor.test(by, fea)$p.value
    test$est <-  coef(lmm)[2]
    test$conf.int <- confint(lmm)[2,]
  }
  else if (test.name == "kendall") {
    tt <- cor.test(by, fea, method="kendall")
    test$p.value <- tt$p.value
    test$est <-  tt$estimate
  }

  if ((test.name == 'chis' | test.name == 'fisher') & (length(levels(fea)) < 3 & length(levels(by)) < 3)){
    t <- table(fea, by)
    test$est <- (t[2,2]/t[1,2])/(t[2,1]/t[1,1])
    test$conf.int <- fisher.test(fea, by, conf.int = T)$conf.int
  }


  est <- get_value(fea.name, "Estimate", result)
  pval <- get_value(fea.name, "P", result)
  confi <- get_value(fea.name, "95% CI", result)

  expect_equal(pval, round(test$p.value, 3))
  if (length(levels(fea)) > 2 | length(levels(by)) > 2) {
    expect_equal(unname(result[result[,"Variable"] == fea.name, "Estimate"]), '-')
    expect_equal(unname(result[result[,"Variable"] == fea.name, "95% CI"]), '-')
  } else {
    expect_equal(est, round(unname(test$est), 2))
    if (test.name == "kendall") expect_length(confi, 0)
    else {
      expect_equal(confi[1], unname(round(test$conf.int[1], 2)))
      expect_equal(confi[2], unname(round(test$conf.int[2], 2)))

    }
  }


}

test_regression <- function (by_name, result, tolerance, binom=F) {

  if (binom) lmm <- glm(reformulate(setdiff(names(mock_data), by_name), by_name), family = binomial, data=mock_data)
  else lmm <- lm(reformulate(setdiff(names(mock_data), by_name), by_name), data=mock_data)

  coefs <- coef(summary(lmm))
  conf  <- confint(lmm)


  rows <- names(mock_data)
  out <- list() # list(name in lmm: c(offset, name in data,))
  for (fea_name in rows){

    if (fea_name == by_name) next

    fea <- mock_data[[fea_name]]
    lvls <- levels(fea)[-1]
    if (length(lvls) > 0) {
      new_values <- seq_along(lvls)
      names(new_values) <- paste0(fea_name, lvls)
      new_values <- lapply(new_values, function(x) c(x, fea_name))
      out <- c(out, as.list(new_values))
    } else {
      out <- c(out, setNames(list(c(0, fea_name)), fea_name))
    }
  }

  for (lmm_name in names(out)) {

        offs <- out[[lmm_name]][1]
        tab_name <- out[[lmm_name]][2]

        est <- get_value(tab_name, "Estimate", result, offset = offs)
        pval <- get_value(tab_name, "P", result, offset = offs)
        confi <- get_value(tab_name, "95% CI", result, offset= offs)

        est.exp  <- coefs[lmm_name, "Estimate"]
        if (binom) p.exp <- coefs[lmm_name, "Pr(>|z|)"]  # or Pr(>|z|) for glm
        else p.exp <- coefs[lmm_name, "Pr(>|t|)"]  # or Pr(>|z|) for glm
        ci.exp  <- conf[lmm_name, ]
        # browser()
        expect_equal(pval, round(p.exp, 3))
        expect_equal(est, round(unname(est.exp), 2))
        expect_equal(confi[1], unname(round(ci.exp[1], 2)))
        expect_equal(confi[2], unname(round(ci.exp[2], 2)))

  }
}




