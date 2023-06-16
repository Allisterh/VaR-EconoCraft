#' Calculate Rolling Value at Risk (VaR) and Expected Shortfall (ES)
#'
#' This function calculates the rolling Value at Risk (VaR) and Expected Shortfall (ES)
#' for a time series of portfolio returns using a specified method ("historical", "gaussian", "t-student").
#'
#' @param portfolio_returns A numeric vector of portfolio returns.
#' @param alpha A numeric value between 0 and 1 representing the significance level.
#' @param method A character string specifying the method to be used ("historical", "gaussian" or "t-student"). Default is 'historical'.
#' @param test_size A numeric value indicating the number of data points to be used for testing. Default is 80% of portfolio_returns length.
#'
#' @return A list containing two numeric vectors: VaR and ES.
#'
#' @examples
#' \dontrun{
#' returns <- rnorm(100, 0.05, 0.02)
#' results <- calculate_rolling_VaR(returns, alpha = 0.05, method = "historical")
#' plot(returns[21:100], type = 'l')
#' lines(results$VaR, col = "red")
#' lines(results$ES, col = "blue")
#' }
#'
#' @seealso \code{\link{calculate_VaR}}
#' @export

calculate_rolling_VaR <- function(portfolio_returns, alpha = 0.05, method = "historical", test_size = floor(0.8 * length(portfolio_returns))) {
  n <- length(portfolio_returns)
  training_size <- n - test_size
  
  if (training_size <= 0) {
    stop("Test size should be smaller than the number of observations.")
  }
  
  # Vectorized function for rolling calculation
  compute_metrics <- function(index) {
    training_set <- portfolio_returns[index:(training_size + index - 1)]
    results <- calculate_VaR(training_set, alpha, method)
    return(c(results$VaR, results$ES))
  }
  
  # Using apply function
  metrics <- t(sapply(1:test_size, compute_metrics))
  
  # Return VaR and ES as a named list
  return(list(VaR = metrics[, 1], ES = metrics[, 2]))
}
