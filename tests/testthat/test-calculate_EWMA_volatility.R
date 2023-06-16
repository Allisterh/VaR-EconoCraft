test_that("calculate_EWMA_volatility computes correct values with default lambda", {
  set.seed(2137)
  returns <- rnorm(10)
  lambda <- 0.94
  
  ewma_vol <- calculate_EWMA_volatility(returns, lambda)
  
  expect_equal(length(ewma_vol), 10)
  expect_true(all(ewma_vol >= 0))
})

test_that("calculate_EWMA_volatility throws an error for non-numeric returns", {
  non_numeric_returns <- c("a", "b", "c")
  lambda <- 0.94
  
  expect_error(calculate_EWMA_volatility(non_numeric_returns, lambda), "portfolio_returns should be a numeric vector.")
})

test_that("calculate_EWMA_volatility throws an error for invalid lambda", {
  returns <- rnorm(10)
  invalid_lambda <- 1.5
  
  expect_error(calculate_EWMA_volatility(returns, invalid_lambda), "lambda should be a numeric value between 0 and 1.")
})

test_that("calculate_EWMA_volatility computes correct values with small input", {
  returns <- c(0.02, -0.01)
  lambda <- 0.94
  
  ewma_vol <- calculate_EWMA_volatility(returns, lambda)
  
  expect_equal(length(ewma_vol), 2)
  expect_true(all(ewma_vol >= 0))
})
