test_that("VaR is negative for losses", {
  set.seed(2137)
  returns <- rnorm(1000, 0, 0.01)
  alpha <- 0.05
  result <- calculate_VaR_gaussian(returns, alpha)
  expect_lt(result$VaR, 0)
})

test_that("ES is negative for losses", {
  set.seed(2137)
  returns <- rnorm(1000, 0, 0.01)
  alpha <- 0.05
  result <- calculate_VaR_gaussian(returns, alpha)
  expect_lt(result$ES, 0)
})

test_that("Exceeds_Realized is positive", {
  set.seed(2137)
  returns <- rnorm(1000, 0, 0.01)
  alpha <- 0.05
  result <- calculate_VaR_gaussian(returns, alpha)
  expect_gt(result$Exceeds_Realized, 0)
})

test_that("Function returns correct structure", {
  set.seed(2137)
  returns <- rnorm(1000, 0, 0.01)
  alpha <- 0.05
  result <- calculate_VaR_gaussian(returns, alpha)
  expect_type(result, "list")
  expect_named(result, c("VaR", "ES", "Exceeds_Realized"))
})

test_that("Error when alpha is outside the range [0, 1]", {
  set.seed(2137)
  returns <- rnorm(1000, 0, 0.01)
  alpha <- 1.1
  expect_error(calculate_VaR_gaussian(returns, alpha))
  
  alpha <- -0.1
  expect_error(calculate_VaR_gaussian(returns, alpha))
})

test_that("VaR and ES are NA when portfolio_returns is empty", {
  returns <- c()
  alpha <- 0.05
  result <- calculate_VaR_gaussian(returns, alpha)
  expect_true(is.na(result$VaR))
  expect_true(is.na(result$ES))
})

test_that("Error when portfolio_returns is not numeric", {
  returns <- c("a", "b", "c")
  alpha <- 0.05
  expect_error(calculate_VaR_gaussian(returns, alpha))
})

test_that("Function handles very small alpha correctly", {
  set.seed(2137)
  returns <- rnorm(1000, 0, 0.01)
  alpha <- 1e-6
  result <- calculate_VaR_gaussian(returns, alpha)
  expect_true(result$VaR < 0)  # We expect a loss in VaR for small alpha
})

test_that("Function handles alpha equal to 0 and 1 correctly", {
  set.seed(2137)
  returns <- rnorm(1000, 0, 0.01)
  alpha <- 0
  result <- calculate_VaR_gaussian(returns, alpha)
  expect_true(result$VaR <= min(returns))
  
  alpha <- 1
  result <- calculate_VaR_gaussian(returns, alpha)
  expect_true(result$VaR >= max(returns))
})

test_that("Expect ES to be NaN and Exceeds_Realized to be 0 when sigma is 0", {
  returns <- rep(0.02, 100)
  alpha <- 0.05
  result <- calculate_VaR_gaussian(returns, alpha)
  expect_true(is.nan(result$ES))
  expect_equal(result$Exceeds_Realized, 0)
})

