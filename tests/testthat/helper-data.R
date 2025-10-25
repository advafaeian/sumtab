set.seed(49)

n = 50

mock_data <- data.frame(
  age = rnorm(n, mean = 45, sd = 12),
  income = rlnorm(n, meanlog = 10, sdlog = 0.5),
  smoker = factor(c(rep("+", 48), rep("-", 2))),
  education = factor(sample(c("HighSchool", "College", "Graduate"), n, replace = TRUE)),
  group = factor(sample(c("Control", "Case"), n, replace = TRUE), levels = c("Control", "Case")),
  sex = factor(sample(c("Male", "Female"), n, replace = TRUE))
)
mock_data$age[sample(1:n, 3)] <- NA
mock_data$income[sample(1:n, 2)] <- NA
