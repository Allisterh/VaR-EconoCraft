test_that("calculate_VaR calls calculate_VaR_historical for method='historical'", {
  set.seed(2137)
  returns <- rnorm(100, 0.05, 0.01)
  alpha <- 0.05
  historical_result <- calculate_VaR(returns, alpha, method = "historical")
  direct_result <- calculate_VaR_historical(returns, alpha)
  expect_equal(historical_result$VaR, direct_result$VaR)
  expect_equal(historical_result$ES, direct_result$ES)
})

test_that("calculate_VaR calls calculate_VaR_gaussian for method='gaussian'", {
  set.seed(2137)
  returns <- rnorm(100, 0.05, 0.01)
  alpha <- 0.05
  gaussian_result <- calculate_VaR(returns, alpha, method = "gaussian")
  direct_result <- calculate_VaR_gaussian(returns, alpha)
  expect_equal(gaussian_result$VaR, direct_result$VaR)
  expect_equal(gaussian_result$ES, direct_result$ES)
})

test_that("calculate_VaR calls calculate_VaR_t_student for method='t-student'", {
  set.seed(2137)
  returns <- rnorm(100, 0.05, 0.01)
  alpha <- 0.05
  t_student_result <- calculate_VaR(returns, alpha, method = "t-student")
  direct_result <- calculate_VaR_t_student(returns, alpha)
  expect_equal(t_student_result$VaR, direct_result$VaR)
  expect_equal(t_student_result$ES, direct_result$ES)
})

test_that("calculate_VaR gives an error for an invalid method", {
  set.seed(2137)
  returns <- rnorm(100, 0.05, 0.01)
  alpha <- 0.05
  expect_error(calculate_VaR(returns, alpha, method = "invalid_method"))
})
