generate_formula = function(y, xs, degrees, interaction_terms = NULL) {
  
  if (length(xs) != length(degrees)) {
    stop('Length of xs must me equal to the length of degrees.')
  }
  
  n_terms = length(xs)
  terms = c()
  
  for (i in 1:n_terms) {
    terms = c(terms, xs[i])
  }
  
  paste0(y, ' ~ ', paste(terms, collapse = ' + '))
  
}
