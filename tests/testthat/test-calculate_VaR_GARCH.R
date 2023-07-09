test_that("VaR and is negative for losses", {
  set.seed(2137)
  returns <- rnorm(1000)
  alpha <- 0.05
  spec = rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)))
  fit = rugarch::ugarchfit(spec, data = returns)
  result <- calculate_VaR_GARCH(fit, alpha)
  expect_lt(result$VaR, 0)
})

test_that("ES is negative for losses", {
  set.seed(2137)
  returns <- rnorm(1000)
  alpha <- 0.05
  spec = rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)))
  fit = rugarch::ugarchfit(spec, data = returns)
  result <- calculate_VaR_GARCH(fit, alpha)
  expect_lt(result$ES, 0)
})

test_that("Exceeds_Realized is positive", {
  set.seed(2137)
  returns <- rnorm(1000)
  alpha <- 0.05
  spec = rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)))
  fit = rugarch::ugarchfit(spec, data = returns)
  result <- calculate_VaR_GARCH(fit, alpha)
  expect_gt(result$Exceeds_Realized, 0)
})

test_that("Function returns correct structure", {
  set.seed(2137)
  returns <- rnorm(1000)
  alpha <- 0.05
  spec = rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)))
  fit = rugarch::ugarchfit(spec, data = returns)
  result <- calculate_VaR_GARCH(fit, alpha)
  expect_type(result, "list")
  expect_named(result, c("VaR", "ES", "Exceeds_Realized"))
})

test_that("Error when alpha is outside the range [0, 1]", {
  set.seed(2137)
  returns <- rnorm(1000)
  alpha <- 1.1
  spec = rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)))
  fit = rugarch::ugarchfit(spec, data = returns)
  expect_error(calculate_VaR_GARCH(fit, alpha))
  
  alpha <- -0.1
  expect_error(calculate_VaR_GARCH(returns, fit, alpha))
})

test_that("Function handles very small alpha correctly", {
  set.seed(2137)
  returns <- rnorm(1000)
  alpha <- 1e-6
  spec = rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)))
  fit = rugarch::ugarchfit(spec, data = returns)
  result <- calculate_VaR_GARCH(fit, alpha)
  expect_true(result$VaR < 0)  # We expect a loss in VaR for small alpha
})

test_that("Function handles alpha equal to 0 and 1 correctly", {
  set.seed(2137)
  returns <- rnorm(1000)
  alpha <- 0
  result <- calculate_VaR_GARCH(fit, alpha)
  expect_true(result$VaR <= min(returns))
  
  alpha <- 1
  result <- calculate_VaR_GARCH(fit, alpha)
  expect_true(result$VaR >= max(returns))
})
