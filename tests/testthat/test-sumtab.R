#devtools::test_active_file("./tests/testthat/test-sumtab.R")
#devtools::load_all()

TOLERANCE = .01

test_that("sumtab is a matrix and has correct columns and rows", {
  captured_output <- capture.output({
    result <- mock_data %>% sumtab()
  })

  expect_true(is.matrix(result))

  expect_equal(colnames(result), short_columns)

  exp_rows <- expected_rows()
  expect_equal(sum(result[,"Variable"] == exp_rows), nrow(result))

})

test_that("sumtab has correct incomplete rows", {
  captured_output <- capture.output({
    result <- mock_data %>% sumtab(complete_rows = F)
  })
  expect_true(is.matrix(result))

  expect_equal(colnames(result), short_columns)

  exp_rows <- expected_rows(complete = F)
  expect_equal(sum(result[,"Variable"] == exp_rows), nrow(result))

})

test_that("sumtab(by discrete with 2 levels) is a matrix and has correct columns", {
  captured_output <- capture.output({
    result <- mock_data %>% sumtab(by = "group")
  })

  expect_true(is.matrix(result))

  expect_equal(colnames(result), long_columns)

})



test_that("sumtab(by discrete with 2 level) has corect descriptive values", {
  captured_output <- capture.output({
    result <- mock_data %>% sumtab(by = "group")
  })

test_descriptive_num("age", "group", "parametric", result, tolerance = TOLERANCE)
test_descriptive_num("income", "group", "non_parametric", result, tolerance = TOLERANCE)

test_descriptive_catecate("smoker", "group", tolerance = TOLERANCE)
test_descriptive_catecate("education", "group", tolerance = TOLERANCE)
})



test_that("sumtab(by discrete with 2 level) has correct inferential values", {
  captured_output <- capture.output({
    result <- mock_data %>% sumtab(by = "group")
  })

  test_inferential("age", "group", "t", result, tolerance = TOLERANCE)
  test_inferential("income", "group", "wilcox", result, tolerance = TOLERANCE)
  test_inferential("smoker", "group", "fisher", result, tolerance = TOLERANCE)
  test_inferential("education", "group", "chis", result, tolerance = TOLERANCE)
  test_inferential("sex", "group", "chis", result, tolerance = TOLERANCE)
})

test_that("sumtab(by discrete with 3 level) has correct inferential values", {
  captured_output <- capture.output({
    result <- mock_data %>% sumtab(by = "education")
  })

  test_inferential("age", "education", "aov", result, tolerance = TOLERANCE)
  test_inferential("income", "education", "kruksal", result, tolerance = TOLERANCE)
  test_inferential("smoker", "education", "fisher", result, tolerance = TOLERANCE)
  test_inferential("group", "education", "chis", result, tolerance = TOLERANCE)
  test_inferential("sex", "education", "chis", result, tolerance = TOLERANCE)
})


test_that("sumtab(by continous) has correct descriptive values", {
  captured_output <- capture.output({
    result <- mock_data %>% sumtab(by = "age")
  })

  test_descriptive_num("income", "age", "non_parametric", result, tolerance = TOLERANCE)
  test_descriptive_num("smoker", "age", "non_parametric", result, tolerance = TOLERANCE)
  test_descriptive_num("education", "age", "parametric", result, tolerance = TOLERANCE)
  test_descriptive_num("group", "age", "parametric", result, tolerance = TOLERANCE)
  test_descriptive_num("sex", "age", "parametric", result, tolerance = TOLERANCE)

})

test_that("sumtab(by continous) has correct inferential values", {
  captured_output <- capture.output({
    result <- mock_data %>% sumtab(by = "age")
  })

  test_inferential("income", "age", "pearsonr", result, tolerance = TOLERANCE)
  test_inferential("smoker", "age", "wilcox", result, tolerance = TOLERANCE)
  test_inferential("education", "age", "aov", result, tolerance = TOLERANCE)
  test_inferential("group", "age", "t", result, tolerance = TOLERANCE)
  test_inferential("sex", "age", "t", result, tolerance = TOLERANCE)

})

test_that("sumtab(by continous, nonparametric) has correct inferential values", {
  captured_output <- capture.output({
    result <- mock_data %>% sumtab(by = "age", reporting_type = "non_parametric")
  })

  test_inferential("income", "age", "kendall", result, tolerance = TOLERANCE)
  test_inferential("smoker", "age", "wilcox", result, tolerance = TOLERANCE)
  test_inferential("education", "age", "kruksal", result, tolerance = TOLERANCE)
  test_inferential("group", "age", "wilcox", result, tolerance = TOLERANCE)
  test_inferential("sex", "age", "wilcox", result, tolerance = TOLERANCE)

})

test_that("sumtab(by discrete, multivar has correct logistic regression inferential values", {
  captured_output <- capture.output({
    result <- mock_data %>% sumtab(by = "group", multivariate = T)
  })
  test_regression ("group", result, tolerance, binom=T)
})

test_that("sumtab(by continous, multivar has correct regression inferential values", {
  captured_output <- capture.output({
    result <- mock_data %>% sumtab(by = "age", multivariate = T)
  })
  test_regression ("age", result, tolerance, binom=F)
})
## test rr and nonparam and multivar

test_that("sumtab(by discrete), caluculates RR correctly", {
  captured_output <- capture.output({
    result <- mock_data %>% sumtab(by = "group", risk_measure = "RR")
  })
  rr.act <- get_value("smoker", "Estimate", result)
  tt <- table(mock_data$smoker, mock_data$group)
  rr.exp <- (tt[2,2]/(tt[2,2] + tt[2,1])) / (tt[1,2]/(tt[1,2] + tt[1,1]))

  expect_equal(round(rr.act, 2), round(rr.exp,2))

})

test_that("sumtab(by discrete, debug=T) runs without error and returns a list", {
  captured_output <- capture.output({
    result <- mock_data %>% sumtab(by = "group", debug = T)
  })

  expect_true(exists("result"))

  expect_true(is.list(result))

})


test_that("sumtab(format_cd with wrong signature errors", {
  expect_error(
    mock_data %>% sumtab(format_cd = function(x) x),
    regexp = "format_cd"
  )
})



