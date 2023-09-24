test_that("Error where length of xs and degrees differs", {
  xs <- c('x1', 'x2')
  d <- c(1, 1, 2)
  expect_error(generate_formula('y', xs, d))
})


test_that("Function works with 1-degree polynomials", {
  xs <- c('x1', 'x2')
  d <- c(1, 1)
  f <- generate_formula('y', xs, d)
  expect_equal(f, 'y ~ x1 + x2')
})