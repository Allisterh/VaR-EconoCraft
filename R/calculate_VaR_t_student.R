#' Calculate Student's t-distribution Value at Risk and Expected Shortfall
#'
#' This function calculates the Value at Risk (VaR) and Expected Shortfall (ES) of a portfolio
#' based on historical returns, assuming the returns follow a Student's t-distribution.
#'
#' @param portfolio_returns A numeric vector of historical portfolio returns.
#' @param alpha A numeric value between 0 and 1 representing the confidence level at which the VaR is computed.
#'
#' @return A list containing three elements:
#' \itemize{
#'   \item \code{VaR}: The Student's t-distribution Value at Risk at the specified confidence level.
#'   \item \code{ES}: The Expected Shortfall, which is the average loss incurred in the worst (1-alpha)% cases.
#'   \item \code{Exceeds_Realized}: The number of historical returns that exceeded the Value at Risk.
#' }
#'
#' @examples
#' # Example with simulated portfolio returns
#' portfolio_returns <- rt(1000, df = 5)
#' alpha <- 0.05
#' calculate_VaR_t_student(portfolio_returns, alpha)
#'

calculate_VaR_t_student <- function(portfolio_returns, alpha) {
  stopifnot((alpha >= 0 & alpha <= 1) & (is.numeric(portfolio_returns) || length(portfolio_returns) == 0))
  
  error_occurred <- FALSE
  
  fit_result = tryCatch({
    suppressWarnings(MASS::fitdistr(portfolio_returns, 't'))
  }, error = function(e) {
    cat('Error in fitting Student\'s t-distribution.\n')
    error_occurred <<- TRUE
    return(NULL)
  })
  
  # Check if an error occurred and return NA values
  if (error_occurred) {
    return(list(VaR = NA, ES = NA, Exceeds_Realized = NA))
  }
  
  m = unname(fit_result$estimate[1])
  s = unname(fit_result$estimate[2])
  df = unname(fit_result$estimate[3])
  VaR = m + s * qt(alpha, df)
  indicator_ES = portfolio_returns < VaR
  exceeds = sum(indicator_ES)
  ES = sum(portfolio_returns * indicator_ES) / sum(indicator_ES)
  return(list(VaR = VaR, ES = ES, Exceeds_Realized = exceeds))
}
