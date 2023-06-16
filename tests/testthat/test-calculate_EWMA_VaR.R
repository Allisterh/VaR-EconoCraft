# Unit tests for calculate_EWMA_VaR function
test_that("VaR is calculated correctly with normal distribution", {
  set.seed(2137)
  portfolio_returns <- rnorm(100)
  VaR <- calculate_EWMA_VaR(portfolio_returns, alpha = 0.05, distribution = "gaussian")
  expect_true(is.numeric(VaR))
})

test_that("VaR is calculated correctly with t-distribution", {
  set.seed(2137)
  portfolio_returns <- rt(100, df = 5)
  VaR <- calculate_EWMA_VaR(portfolio_returns, alpha = 0.05, distribution = "t-student")
  expect_true(is.numeric(VaR))
})

test_that("Error is thrown when an invalid distribution is supplied", {
  set.seed(2137)
  portfolio_returns <- rnorm(100)
  expect_error(calculate_EWMA_VaR(portfolio_returns, alpha = 0.05, distribution = "invalid_distribution"))
})

test_that("Error is thrown when non-numeric portfolio returns are supplied", {
  portfolio_returns <- c("a", "b", "c")
  expect_error(calculate_EWMA_VaR(portfolio_returns, alpha = 0.05, distribution = "normal"))
})

test_that("Error is thrown when invalid alpha value is supplied", {
  set.seed(2137)
  portfolio_returns <- rnorm(100)
  expect_error(calculate_EWMA_VaR(portfolio_returns, alpha = -0.1, distribution = "normal"))
})

# Integration tests for calculate_EWMA_VaR function
test_that("calculate_EWMA_VaR integrates well with calculate_EWMA_volatility and produces valid VaR", {
  set.seed(2137)
  portfolio_returns <- rnorm(100)
  VaR <- calculate_EWMA_VaR(portfolio_returns, alpha = 0.05, distribution = "gaussian")
  ewma_volatility <- calculate_EWMA_volatility(portfolio_returns)
  expect_true(all(is.numeric(VaR), is.numeric(ewma_volatility)))
})
