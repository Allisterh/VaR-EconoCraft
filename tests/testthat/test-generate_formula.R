test_that("Error where length of xs and degrees differs", {
  xs <- c('x1', 'x2')
  d <- c(1, 1, 2)
  expect_error(generate_formula('y', xs, d))
})

test_that("Error when degrees is not numeric vector of positive integers", {
  xs <- c('x1', 'x2')
  d1 <- c(1, -1)
  expect_error(generate_formula('y', xs, d1))
  
  d2 <- c(1, 0)
  expect_error(generate_formula('y', xs, d2))
  
  d3 <- c('1', '1')
  expect_error(generate_formula('y', xs, d3))
  
  d4 <- c(1.1, 1)
  expect_error(generate_formula('y', xs, d4))
})

test_that("Function works with 1-degree polynomials", {
  xs <- c('x1', 'x2')
  d <- c(1, 1)
  f <- generate_formula('y', xs, d)
  expect_equal(f, 'y ~ x1 + x2')
})

test_that("Function works with higher degree polynomials", {
  xs <- c('x1', 'x2')
  d <- c(2, 3)
  f <- generate_formula('y', xs, d)
  expect_equal(f, 'y ~ x1 + I(x1^2) + x2 + I(x2^2) + I(x2^3)')
})