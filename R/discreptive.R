handle_numcate_dis <- function(nume, cate, param){

  test <- list()
  if (param){
    print("reporting mean±sd")

    total.mean <- mean(nume, na.rm=T)
    total.sd <- sd(nume, na.rm = T)
    test$outer <- total.mean
    test$inner <- total.sd

    means.by.cate <- tapply(nume, cate, mean, na.rm=T)
    sds.by.cate <- tapply(nume, cate, sd, na.rm=T)
    test$numcate <- length(means.by.cate)
    test$outer <- c(total=test$outer, means.by.cate)
    test$inner <- c(total=test$inner, sds.by.cate)
  } else {
    print("reporting median±IQR")

    total.median <- median(nume, na.rm=T)
    total.iqr <- IQR(nume, na.rm = T)
    test$outer <- total.median
    test$inner <- total.iqr

    medians.by.cate <- tapply(nume, cate, mean, na.rm=T)
    iqrs.by.cate <- tapply(nume, cate, IQR, na.rm=T)
    test$numcate <- length(medians.by.cate)
    test$outer <- c(total=total.median, medians.by.cate)
    test$inner <- c(total=total.iqr, iqrs.by.cate)
  }
  return(test)
}

handle_numnum_dis <- function(feature, param){
  test <- list()
  if (param) {
    print("reporting mean±sd")
    test$outer <- mean(feature, na.rm=T)
    test$inner <- sd(feature, na.rm=T)
  } else {
    print("reporting median±IQR")
    test$outer <- median(feature, na.rm=T)
    test$inner <- IQR(feature, na.rm=T)
  }
  return(test)
}

handle_catecate_dis <- function(feature, response){
  test <- list()

  total.feat <- table(feature)
  ptotal.feat <- prop.table(total.feat) * 100

  if (length(response) == 0){
    test$outer <- total.feat
    test$inner <- ptotal.feat
    test$numfeat <- nrow(total.feat)
    test$numresp <- 0
    return(test)
  }

  tables <- table(feature, response)
  tables <- table(feature, response)
  ptables <- prop.table(tables, 2) * 100

  test$numfeat <- nrow(tables)
  test$numresp <- ncol(tables)

  test$outer <- cbind(total.feat, tables)
  test$inner <- cbind(ptotal.feat, ptables)

  return(test)
}
