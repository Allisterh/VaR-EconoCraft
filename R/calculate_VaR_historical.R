#' Calculate Value at Risk and Expected Shortfall
#'
#' This function calculates the Value at Risk (VaR) and Expected Shortfall (ES) of a portfolio
#' based on historical returns using the historical simulation method.
#'
#' @param portfolio_returns A numeric vector of historical portfolio returns.
#' @param alpha A numeric value between 0 and 1 representing the confidence level at which the VaR is computed.
#' 
#' @return A list containing three elements:
#' \itemize{
#'   \item \code{VaR}: The Value at Risk at the specified confidence level.
#'   \item \code{ES}: The Expected Shortfall, which is the average loss incurred in the worst (1-alpha)% cases.
#'   \item \code{Exceeds_Realized}: The number of historical returns that exceeded the Value at Risk.
#' }
#'
#' @examples
#' # Example with simulated portfolio returns
#' portfolio_returns <- rnorm(1000, 0, 0.01)
#' alpha <- 0.05
#' calculate_VaR_historical(portfolio_returns, alpha)

calculate_VaR_historical <- function(portfolio_returns, alpha) {
  VaR = quantile(portfolio_returns, alpha, names = FALSE)
  indicator_ES = portfolio_returns < VaR
  sum_indicator_ES = sum(indicator_ES)
  ES = sum(portfolio_returns * indicator_ES) / sum_indicator_ES
  return(list(VaR = VaR, ES = ES, Exceeds_Realized = sum_indicator_ES))
}