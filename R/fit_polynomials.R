fit_polynomials = function(data, y, xs, degrees, interaction_terms = NULL) {
  
  n_rows = prod(degrees)
  
  grid_formula = c()
  
  for (i in 1:length(xs)) {
    grid_formula = c(grid_formula, paste0(xs[i], ' = 1:', degrees[i]))
  }
  
  grid_formula = paste(grid_formula, collapse = ', ')
  
  results = data.frame(
    eval(parse(text = paste0('expand.grid(', grid_formula, ')')))
    , AIC = NA
    , BIC = NA
    , R2 = NA
    , RMSE = NA
    , MAE = NA
    , JB = NA
    , GQ = NA
    , DW = NA
    , RESET = NA
  )
  
  return(results)
}