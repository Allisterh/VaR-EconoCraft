#' Calculate Value-at-Risk (VaR) for different time horizons using Monte Carlo methods.
#'
#' This function calculates the Value-at-Risk (VaR) at different time horizons using a Monte Carlo simulation. 
#' Returns generating process is provided by the user with additional parameters. It calculates the VaR at each time horizon 
#' by generating N different return paths of h days, and then taking the cumulative sum of returns over each path.
#' It then finds the alpha-quantile of these cumulative returns, which represents the VaR at the given significance level 
#' and time horizon.
#'
#' @param H An integer specifying the maximal time horizon. Default is 5.
#' @param N An integer specifying the number of simulated paths for each time horizon. Default is 1e4.
#' @param alpha A numeric value between 0 and 1 specifying the significance level for the VaR. Default is 0.05.
#' @param method A character string specifying the method for generating returns. Currently, only 'gaussian' 
#' is supported, which means returns are assumed to follow a normal distribution.
#' @param params A list of additional parameters. If method = guassian
#'
#' @return A numeric vector of length H containing the VaR at each time horizon, then params must contain mean and sd.
#' @export
#'
#' @examples
#' # To calculate the VaR at 5 different horizons assuming zero mean and unit standard deviation returns:
#' scale_VaR_monte_carlo(H = 5, N = 1e4, alpha = 0.05, method = 'gaussian', params = list(mean = 0, sd = 1))


scale_VaR_monte_carlo = function(H = 5, N = 1e4, alpha = 0.05, method, params = NULL) {
  
  if(!is.numeric(H) || H <= 0 || floor(H) != H) {
    stop("'H' should be a positive integer")
  }
  
  if(!is.numeric(N) || N <= 0 || floor(N) != N) {
    stop("'N' should be a positive integer")
  }
  
  if(!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("'alpha' should be a numeric value between 0 and 1")
  }
  
  if(!is.character(method) || !(method %in% c('gaussian'))) {
    stop("'method' should be one of 'gaussian'")
  }
  
  VaR_H = numeric(H)
  
  for (h in 1:H) {
    if (method == 'gaussian') {
      simulated_returns = matrix(rnorm(N*h, params[['mean']], params[['sd']]), nrow = N)
      simulated_returns = apply(simulated_returns, 1, function(x) cumsum(x)[h])
    }
    VaR_H[h] = quantile(simulated_returns, alpha, names = FALSE)
  }
  
  return(VaR_H)
}
