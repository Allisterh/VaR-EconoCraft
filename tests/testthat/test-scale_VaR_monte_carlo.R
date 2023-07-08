test_that("function returns a numeric vector", {
  result <- scale_VaR_monte_carlo(H = 5, N = 100, alpha = 0.05, method = 'gaussian', params = list(mean = 0, sd = 1))
  expect_type(result, "double")
})

test_that("function returns a vector of correct length", {
  H = 5
  result <- scale_VaR_monte_carlo(H = H, N = 100, alpha = 0.05, method = 'gaussian', params = list('mean' = 0, 'sd' = 1))
  expect_equal(length(result), H)
})

test_that("function throws error when alpha is not between 0 and 1", {
  expect_error(scale_VaR_monte_carlo(H = 5, N = 100, alpha = -0.05, method = 'gaussian', params = list('mean' = 0, 'sd' = 1)))
  expect_error(scale_VaR_monte_carlo(H = 5, N = 100, alpha = 1.05, method = 'gaussian', params = list('mean' = 0, 'sd' = 1)))
})

test_that("function throws error when H is not a positive integer", {
  expect_error(scale_VaR_monte_carlo(H = -1, N = 100, alpha = 0.05, method = 'gaussian', params = list('mean' = 0, 'sd' = 1)))
  expect_error(scale_VaR_monte_carlo(H = 2.5, N = 100, alpha = 0.05, method = 'gaussian', params = list('mean' = 0, 'sd' = 1)))
  expect_error(scale_VaR_monte_carlo(H = "a", N = 100, alpha = 0.05, method = 'gaussian', params = list('mean' = 0, 'sd' = 1)))
})

test_that("function throws error when N is not a positive integer", {
  expect_error(scale_VaR_monte_carlo(H = 5, N = -100, alpha = 0.05, method = 'gaussian', params = list('mean' = 0, 'sd' = 1)))
  expect_error(scale_VaR_monte_carlo(H = 5, N = 100.5, alpha = 0.05, method = 'gaussian', params = list('mean' = 0, 'sd' = 1)))
  expect_error(scale_VaR_monte_carlo(H = 5, N = "b", alpha = 0.05, method = 'gaussian', params = list('mean' = 0, 'sd' = 1)))
})
