handle_numcate_inf <- function(nume, cate, param, numcate){

  if (param){
    if (numcate>2) {
      print("ANOVA")
      test$p.value <- unlist(summary(aov(nume ~ cate)))["Pr(>F)1"]
    }else{
      print("t test")
      test <- t.test(nume ~ cate)
      test$est <- test$estimate[2] - test$estimate[1]
    }
  }

  if (!param){
    if (numcate>2) {
      print("Kruskal-Wallis")
      test <- kruskal.test(nume ~ cate)
    } else {
      print("wilcox test")
      test <- wilcox.test(nume ~ cate, conf.int = T)
      test$est <- test$estimate
    }
  }

  return(test[c("est", "conf.int", "p.value")])
}

handle_numnum_inf <- function(response, feature, lmm, param){
  test <- list()
  test$est <-  coef(lmm)[2]
  test$conf.int <- confint(lmm)[2,]
  if (param){
    print("pearson regression")
    test$p.value <- cor.test(response, feature)$p.value
  }
  if (!param){
    print("kendall regression")
    test$p.value <- cor.test(response, feature, method="kendall")$p.value
  }
  return(test)
}

handle_catecate_inf <- function(feature, response, tables){
  param <- sum(tables < 5) == 0

  if (param){
    print("chisq test")
    test <- chisq.test(feature, response)
    test$conf.int <- fisher.test(feature, response)$conf.int
  }
  if (!param){
    print("fisher test")
    test <- fisher.test(feature, response)
  }

  est <- tables[2,]/tables[1,] ## OR
  test$est <- est[2]/est[1]

  return(test)
}

doing_multivariate <- function(isresnum, data, by){
  if (isresnum) {
    family <- "gaussian"
  } else {
    family <- "binomial"
  }
  model <- glm(as.formula(paste(by,"~.")), family=family, data=data)
  print(summary(model))
  return(model)
}

extract_multivariate <- function(name, model, isresnum){
  test <- list()
  index <- which(grepl(name, names(coef(model))))
  test$est <- coef(model)[index]
  test$conf.int <- confint(model)[index,]
  test$p.value <- coef(summary(model))[index,ifelse(isresnum, 'Pr(>|t|)', 'Pr(>|z|)')]
  return(test)
}

handle_all_inf <- function(test, name, feature, response, isfeatnum, isresnum, numfeat, numresp, param, multivariate, model){
  test.temp <- list()

  if (multivariate){
    test.temp <- extract_multivariate(name, model, isresnum)
  }else if(isresnum & isfeatnum){
    test.temp <- handle_numnum_inf(response, feature, test$lmm, param)
  }else if(isresnum & !isfeatnum){
    test.temp <- handle_numcate_inf(response, feature, param, numfeat)
  }else if(!isresnum & isfeatnum){
    test.temp <- handle_numcate_inf(feature, response, param, numresp)
  }else if(!isresnum & !isfeatnum){
    test.temp <- handle_catecate_inf(feature, response, test$outer)
  }

  test <- c(test.temp, test)
  test$ci <- test$conf.int
  test$p <- test$p.value
  test$p <- handle_ps(test$p)
  test <- test[c("inner", "outer", "est", "ci", "p")] ### getting rid of default tests elements
  return(test)
}
