library(testthat)
set.seed(49)

test_that("fmt_num_default rounds and formats correctly", {
  expect_equal(fmt_num_default(3.14159, list(param_name = "est")), "3.14")
  expect_equal(fmt_num_default(3.14159, list(param_name = "p")), "3.142")
  expect_equal(fmt_num_default(2, list(param_name = "est")), "2.00")
  expect_type(fmt_num_default(2.5, list(param_name = "est")), "character") # always returns character
})

test_that("handle_ps formats p-values correctly", {
  expect_equal(handle_ps(0.00001), "<0.0001****")
  expect_equal(handle_ps(0.0005), "<0.001***")
  expect_equal(handle_ps(0.009), "0.009**")
  expect_equal(handle_ps(0.041), "0.041*")
  expect_equal(handle_ps(0.534), "0.534")
  expect_equal(handle_ps(1), "<1")
  expect_equal(handle_ps(0.99999), "<1")
})

test_that("is_numcat_param respects reporting_type", {
  x <- rnorm(10)
  g <- rep(c("A","B"), each = 5)
  expect_false(is_numcat_param(x, g, reporting_type = "non_parametric"))
  expect_true(is_numcat_param(x, g, reporting_type = "parametric"))
})

test_that("is_numcat_param correctly decides parametric vs non_parametric", {
  x <- rnorm(20, mean=5)
  g <- rep(c("A","B","C","D"), each=5)
  expect_true(is_numcat_param(x, g, reporting_type = "auto"))

  x <- c(1,2,1)
  g <- c("A","B", "A")
  expect_false(is_numcat_param(x, g, reporting_type = "auto"))

})

test_that("is_numnum_param respects reporting_type", {
  x <- rnorm(20)
  expect_false(is_numnum_param(x, reporting_type = "non_parametric"))
  expect_true(is_numnum_param(x, reporting_type = "parametric"))
})

test_that("is_numnum_param correctly decides parametric vs non_parametric", {
  df <- data.frame(y = rnorm(10), x = rnorm(10))
  expect_true(is_numnum_param(df$x, reporting_type = "auto"))
  lmm = lm(y ~ x, data=df)
  expect_true(is_numnum_param(df$x, lmm = lmm, reporting_type = "auto"))

  df <- data.frame(y = c(rep(3, 9), 2), x =c(3, rep(4, 9)))
  expect_false(is_numnum_param(df$x, reporting_type = "auto"))
  lmm = lm(y ~ x, data=df)
  expect_false(is_numnum_param(df$x, lmm=lmm, reporting_type = "auto"))
})
