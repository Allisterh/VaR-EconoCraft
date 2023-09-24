test_that("Function returns data.frame with proper columns", {
  y = 'mpg'
  xs = c('disp', 'hp')
  degrees = c(1, 2)
  
  result = fit_polynomials(data = mtcars, y = y, xs = xs, degrees = degrees, interaction_terms = NULL)
  
  expect_s3_class(results, 'data.frame')
  cols_model = colnames(result)[(length(xs) + 1):ncol(result)]
  expect_equal(colnames(cols_model), c('AIC', 'BIC', 'R2', 'RMSE', 'MAE', 'JB', 'GQ', 'DW', 'RESET'))
})

test_that("Function return correct number of rows", {
  y = 'mpg'
  xs = c('disp', 'hp', 'drat')
  degrees = c(2, 2, 3)
  
  result = fit_polynomials(data = mtcars, y = y, xs = xs, degrees = degrees, interaction_terms = NULL)
  expect_equal(nrow(result), prod(degrees))
})