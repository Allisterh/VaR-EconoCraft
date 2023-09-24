generate_formula = function(y, xs, degrees, interaction_terms = NULL) {
  
  if (!is.numeric(degrees) | !all(degrees > 0) | !all(round(degrees, 0) == degrees)) {
    stop("Degrees must be numeric vector of positive integers.")
  }
  
  if (length(xs) != length(degrees)) {
    stop('Length of xs must me equal to the length of degrees.')
  }
  
  n_terms = length(xs)
  terms = c()
  
  for (i in 1:n_terms) {
    for (j in 1:degrees[i]) {
      if (j == 1) {
        terms = c(terms, xs[i])
      } else {            
        terms = c(terms, paste0('I(', xs[i], '^', j, ')'))
      }
    }
  }
  
  paste0(y, ' ~ ', paste(terms, collapse = ' + '))
  
}
