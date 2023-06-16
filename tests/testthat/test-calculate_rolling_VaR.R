# Unit tests for calculate_rolling_VaR function
test_that("calculate_rolling_VaR returns correct length of VaR and ES", {
  returns <- rnorm(100, 0.05, 0.02)
  alpha <- 0.05
  test_size <- 80
  result <- calculate_rolling_VaR(returns, alpha, test_size = test_size)
  expect_equal(length(result$VaR), test_size)
  expect_equal(length(result$ES), test_size)
})

test_that("calculate_rolling_VaR returns an error for invalid test_size", {
  returns <- rnorm(100, 0.05, 0.02)
  alpha <- 0.05
  expect_error(calculate_rolling_VaR(returns, alpha, test_size = 101))
})

# Integration tests for calculate_rolling_VaR function
test_that("calculate_rolling_VaR integrates correctly with calculate_VaR", {
  set.seed(2137)
  returns <- rnorm(100, 0.05, 0.02)
  alpha <- 0.05
  test_size <- 80
  result <- calculate_rolling_VaR(returns, alpha, test_size = test_size)
  single_result <- calculate_VaR(returns[1:20], alpha)
  expect_equal(result$VaR[1], single_result$VaR)
})
