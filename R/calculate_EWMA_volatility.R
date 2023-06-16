#' Calculate Exponentially Weighted Moving Average (EWMA) Volatility
#'
#' This function calculates the exponentially weighted moving average (EWMA) volatility of a given set of portfolio returns.
#' The EWMA model is a common approach to estimating the volatility in finance, which assigns exponentially decreasing weights to past squared returns.
#' Note that this function is not exported and is intended for internal use within the package.
#'
#' @param portfolio_returns A numeric vector of portfolio returns.
#' @param lambda A numeric value between 0 and 1 representing the smoothing parameter. The default value is 0.94.
#'
#' @return A numeric vector of the same length as the input, representing the EWMA volatility estimate at each point in time.
#'
#' @examples
#' \dontrun{
#' # Sample portfolio returns
#' portfolio_returns <- c(-0.02, 0.01, -0.03, 0.04, -0.01)
#' 
#' # Calculate EWMA volatility with default lambda
#' vol_ewma <- calculate_EWMA_volatility(portfolio_returns)
#' 
#' # Calculate EWMA volatility with custom lambda
#' vol_ewma_custom_lambda <- calculate_EWMA_volatility(portfolio_returns, lambda = 0.90)
#' }
#'

calculate_EWMA_volatility <- function(portfolio_returns, lambda = 0.94) {
  # Check if portfolio_returns is numeric
  if (!is.numeric(portfolio_returns)) {
    stop("portfolio_returns should be a numeric vector.")
  }
  
  # Check if lambda is numeric and within the range (0, 1)
  if (!is.numeric(lambda) || lambda <= 0 || lambda >= 1) {
    stop("lambda should be a numeric value between 0 and 1.")
  }
  
  # Initialize the vol_ewma vector
  vol_ewma = rep(0, length(portfolio_returns))
  
  # Loop through portfolio_returns
  for (i in seq_along(portfolio_returns)) {
    if (i == 1) {
      vol_ewma[i] <- var(portfolio_returns)
    } else {
      vol_ewma[i] <- lambda * vol_ewma[i-1] + (1 - lambda) * portfolio_returns[i-1]^2
    }
  }
  
  # Return the vol_ewma vector
  return(vol_ewma)
}
