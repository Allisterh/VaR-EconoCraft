test_that("VaR is negative for losses", {
  set.seed(2137)
  returns <- rt(1000, df = 5)
  alpha <- 0.05
  result <- calculate_VaR_t_student(returns, alpha)
  expect_lt(result$VaR, 0)
})

test_that("ES is negative for losses", {
  set.seed(2137)
  returns <- rt(1000, df = 5)
  alpha <- 0.05
  result <- calculate_VaR_t_student(returns, alpha)
  expect_lt(result$ES, 0)
})

test_that("Exceeds_Realized is positive", {
  set.seed(2137)
  returns <- rt(1000, df = 5)
  alpha <- 0.05
  result <- calculate_VaR_t_student(returns, alpha)
  expect_gt(result$Exceeds_Realized, 0)
})

test_that("Function returns correct structure", {
  set.seed(2137)
  returns <- rt(1000, df = 5)
  alpha <- 0.05
  result <- calculate_VaR_t_student(returns, alpha)
  expect_type(result, "list")
  expect_named(result, c("VaR", "ES", "Exceeds_Realized"))
})

test_that("Error when alpha is outside the range [0, 1]", {
  returns <- rt(1000, df = 5)
  alpha <- 1.1
  expect_error(calculate_VaR_t_student(returns, alpha))
  
  alpha <- -0.1
  expect_error(calculate_VaR_t_student(returns, alpha))
})

test_that("VaR and ES are NA when portfolio_returns is empty", {
  returns <- c()
  alpha <- 0.05
  result <- calculate_VaR_t_student(returns, alpha)
  expect_true(is.na(result$VaR))
  expect_true(is.na(result$ES))
})

test_that("Error when portfolio_returns is not numeric", {
  returns <- c("a", "b", "c")
  alpha <- 0.05
  expect_error(calculate_VaR_t_student(returns, alpha))
})

test_that("Function handles very small alpha correctly", {
  set.seed(2137)
  returns <- rt(1000, df = 5)
  alpha <- 1e-6
  result <- calculate_VaR_t_student(returns, alpha)
  expect_true(result$VaR < 0)  # We expect a loss in VaR for small alpha
})

test_that("Function handles alpha equal to 0 and 1 correctly", {
  set.seed(2137)
  returns <- rt(1000, df = 5)
  alpha <- 0
  result <- calculate_VaR_t_student(returns, alpha)
  expect_true(result$VaR <= min(returns))
  
  alpha <- 1
  result <- calculate_VaR_t_student(returns, alpha)
  expect_true(result$VaR >= max(returns))
})

test_that("Function handles well the df being high", {
  set.seed(2137)
  returns <- rnorm(1000)  # Normally distributed, where t distribution converges as df -> inf
  alpha <- 0.05
  result <- calculate_VaR_t_student(returns, alpha)
  # Should be close to normal VaR but not necessarily very close
  expect_true(abs(result$VaR - qnorm(alpha)) < 0.05)
})

test_that("VaR and ES are NA when df cannot be estimated", {
  returns <- rep(0.02, 10)  # Not enough data to estimate df
  alpha <- 0.05
  result <- calculate_VaR_t_student(returns, alpha)
  expect_true(is.na(result$VaR))
  expect_true(is.na(result$ES))
})
