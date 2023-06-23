test_that("VaR is calculated correctly with normal distribution", {
  set.seed(2137)
  returns <- rnorm(100)
  alpha <- 0.05
  result <- calculate_VaR_EWMA(returns, alpha, distribution = "gaussian")
  expect_true(is.numeric(result$VaR))
  expect_true(is.numeric(result$ES))
  expect_true(is.numeric(result$Exceeds_Realized))
})

test_that("VaR is calculated correctly with t-distribution", {
  set.seed(2137)
  returns <- rt(100, df = 5)
  alpha <- 0.05
  result <- calculate_VaR_EWMA(returns, alpha, distribution = "t-student")
  expect_true(is.numeric(result$VaR))
  expect_true(is.numeric(result$ES))
  expect_true(is.numeric(result$Exceeds_Realized))
})

test_that("Error is thrown when an invalid distribution is supplied", {
  set.seed(2137)
  returns <- rnorm(100)
  alpha <- 0.05
  expect_error(calculate_VaR_EWMA(returns, alpha, distribution = "invalid_distribution"))
})

test_that("VaR is negative for losses", {
  set.seed(2137)
  returns <- rnorm(1000, 0, 0.01)
  alpha <- 0.05
  result <- calculate_VaR_EWMA(returns, alpha, distribution = "gaussian")
  expect_lt(result$VaR, 0)
  result <- calculate_VaR_EWMA(returns, alpha, distribution = "t-student")
  expect_lt(result$VaR, 0)
})

test_that("ES is negative for losses", {
  set.seed(2137)
  returns <- rnorm(1000, 0, 0.01)
  alpha <- 0.05
  result <- calculate_VaR_EWMA(returns, alpha, distribution = "gaussian")
  expect_lt(result$ES, 0)
  result <- calculate_VaR_EWMA(returns, alpha, distribution = "t-student")
  expect_lt(result$ES, 0)
})

test_that("Exceeds_Realized is positive", {
  set.seed(2137)
  returns <- rnorm(1000, 0, 0.01)
  alpha <- 0.05
  result <- calculate_VaR_EWMA(returns, alpha, distribution = "gaussian")
  expect_gt(result$Exceeds_Realized, 0)
  result <- calculate_VaR_EWMA(returns, alpha, distribution = "t-student")
  expect_gt(result$Exceeds_Realized, 0)
})

test_that("Function returns correct structure", {
  set.seed(2137)
  returns <- rnorm(1000, 0, 0.01)
  alpha <- 0.05
  result <- calculate_VaR_EWMA(returns, alpha, distribution = "gaussian")
  expect_type(result, "list")
  expect_named(result, c("VaR", "ES", "Exceeds_Realized"))
  result <- calculate_VaR_EWMA(returns, alpha, distribution = "t-student")
  expect_type(result, "list")
  expect_named(result, c("VaR", "ES", "Exceeds_Realized"))
})

test_that("Error when alpha is outside the range [0, 1]", {
  set.seed(2137)
  returns <- rnorm(1000, 0, 0.01)
  alpha <- 1.1
  expect_error(calculate_VaR_EWMA(returns, alpha, distribution = "gaussian"))
  
  alpha <- -0.1
  expect_error(calculate_VaR_EWMA(returns, alpha, distribution = "gaussian"))
})

test_that("Error when portfolio_returns is empty", {
  returns <- c()
  alpha <- 0.05
  expect_error(calculate_VaR_EWMA(returns, alpha, distribution = "gaussian"))
})

test_that("Error is thrown when non-numeric portfolio returns are supplied", {
  portfolio_returns <- c("a", "b", "c")
  alpha <- 0.05
  expect_error(calculate_VaR_EWMA(portfolio_returns, alpha, distribution = "normal"))
})

test_that("Function handles alpha equal to 0 and 1 correctly", {
  set.seed(2137)
  returns <- rnorm(1000, 0, 0.01)
  alpha <- 0
  result <- calculate_VaR_EWMA(returns, alpha, distribution = "gaussian")
  expect_true(result$VaR <= min(returns))
  
  alpha <- 1
  result <- calculate_VaR_EWMA(returns, alpha, distribution = "gaussian")
  expect_true(result$VaR >= max(returns))
})
