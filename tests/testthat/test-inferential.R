library(testthat)

set.seed(123)
num <- rnorm(10)
cat <- rep(c("A","B"), each=5)
response <- rnorm(10)
feature <- rnorm(10)
tables <- matrix(c(10, 2, 3, 15, 4, 5), nrow=2, byrow=TRUE)

test_that("handle_numcate_inf works", {
  captured_output <- capture.output({
    res <- handle_numcate_inf(num, cat, param=TRUE, numcate=2)
  })

  expect_true(all(c("est","conf.int","p.value") %in% names(res)))
  expect_type(res$est, "double")

  captured_output <- capture.output({
    res2 <- handle_numcate_inf(num, cat, param=FALSE, numcate=2)
  })

  expect_true(all(c("est","conf.int","p.value") %in% names(res2)))
})

test_that("handle_numnum_inf works", {
  lmm <- lm(response ~ feature)
  captured_output <- capture.output({
    res <- handle_numnum_inf(response, feature, lmm, param=TRUE)
  })
  expect_true(all(c("est","conf.int","p.value", "name") %in% names(res)))
  captured_output <- capture.output({
    res2 <- handle_numnum_inf(response, feature, lmm, param=FALSE)
  })
  expect_true(all(c("est","conf.int","p.value", "name") %in% names(res2)))
})

test_that("handle_catecate_inf works", {
  captured_output <- capture.output({
    res <- handle_catecate_inf(factor(c(0,1,0,1,0,1)), factor(c(0,0,1,1,0,1)), tables, risk_measure="RR")
  })
  expect_true(all(c("est","conf.int") %in% names(res)))
  captured_output <- capture.output({
    res2 <- handle_catecate_inf(factor(c(0,1)), factor(c(1,0)), tables, risk_measure="OR")
  })
  expect_true("est" %in% names(res2))
})

test_that("doing_multivariate and extract_multivariate work", {
  data <- data.frame(y = rnorm(10), x1 = rnorm(10), x2 = rnorm(10))

  captured_output <- capture.output({
    model <- doing_multivariate(isresnum=TRUE, data, by="y")
    res <- extract_multivariate("x1", model, isresnum=TRUE)
  })

  expect_true(all(c("est","conf.int","p.value") %in% names(res)))
})

test_that("handle_all_inf returns proper structure", {
  test_list <- list(lmm=lm(response ~ feature), outer=tables, inner=NULL)
  captured_output <- capture.output({
    res <- handle_all_inf(test_list, name="feature", feature, response,
                          isfeatnum=TRUE, isresnum=TRUE, numfeat=2, numresp=2,
                          param=TRUE, multivariate=FALSE, model=NULL, risk_measure="RR")
  })

  expect_true(all(c("est","ci","p","inner","outer") %in% names(res)))
})
