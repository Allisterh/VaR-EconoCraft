test_that("VaR is correctly computed with simple data", {
  returns <- c(-0.05, -0.02, 0.01, 0.03, 0.05)
  alpha <- 0.05
  result <- calculate_VaR_historical(returns, alpha)
  expect_equal(result$VaR, -0.044)
})

test_that("ES is correctly computed with simple data", {
  returns <- c(-0.05, -0.02, 0.01, 0.03, 0.05)
  alpha <- 0.05
  result <- calculate_VaR_historical(returns, alpha)
  expect_equal(result$ES, -0.05)
})

test_that("Exceeds_Realized is correctly computed with simple data", {
  returns <- c(-0.05, -0.02, 0.01, 0.03, 0.05)
  alpha <- 0.05
  result <- calculate_VaR_historical(returns, alpha)
  expect_equal(result$Exceeds_Realized, 1)
})

test_that("Function returns correct structure", {
  returns <- c(-0.05, -0.02, 0.01, 0.03, 0.05)
  alpha <- 0.05
  result <- calculate_VaR_historical(returns, alpha)
  expect_type(result, "list")
  expect_named(result, c("VaR", "ES", "Exceeds_Realized"))
})

test_that("VaR and ES are NA when portfolio_returns is empty", {
  returns <- c()
  alpha <- 0.05
  result <- calculate_VaR_historical(returns, alpha)
  expect_true(is.na(result$VaR))
  expect_true(is.na(result$ES))
})

test_that("Error when alpha is outside the range [0, 1]", {
  returns <- c(-0.05, -0.02, 0.01, 0.03, 0.05)
  alpha <- 1.1
  expect_error(calculate_VaR_historical(returns, alpha))
  
  alpha <- -0.1
  expect_error(calculate_VaR_historical(returns, alpha))
})

test_that("Error when portfolio_returns is not numeric", {
  returns <- c("a", "b", "c")
  alpha <- 0.05
  expect_error(calculate_VaR_historical(returns, alpha))
})

test_that("Function handles very small alpha correctly", {
  returns <- rnorm(10000, 0, 0.01)
  alpha <- 1e-6
  result <- calculate_VaR_historical(returns, alpha)
  expect_true(result$VaR < 0) # We expect a loss in VaR for small alpha
})

test_that("Function handles alpha equal to 0 and 1 correctly", {
  returns <- c(-0.05, -0.02, 0.01, 0.03, 0.05)
  alpha <- 0
  result <- calculate_VaR_historical(returns, alpha)
  expect_equal(result$VaR, min(returns))
  
  alpha <- 1
  result <- calculate_VaR_historical(returns, alpha)
  expect_equal(result$VaR, max(returns))
})