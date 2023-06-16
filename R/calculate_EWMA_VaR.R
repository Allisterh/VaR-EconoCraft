#' Calculate VaR using Exponentially Weighted Moving Average (EWMA) method
#'
#' @description
#' This function calculates Value at Risk (VaR) of portfolio returns using the EWMA method for
#' estimating volatility, with an option to assume normal or Student's t-distribution.
#'
#' @param portfolio_returns A numeric vector of portfolio returns.
#' @param alpha A numeric value between 0 and 1, representing the significance level.
#' @param distribution A character string specifying the distribution ('gaussian' or 't-student').
#' 
#' @return
#' Returns a numeric value representing the VaR.
#' 
#' @examples
#' \dontrun{
#' portfolio_returns <- rnorm(1000)
#' 
#' # Calculate VaR using normal distribution
#' VaR_EWMA_normal <- calculate_EWMA_VaR(portfolio_returns, alpha = 0.05, distribution = "gaussian")
#' 
#' # Calculate VaR using t-distribution
#' VaR_EWMA_t <- calculate_EWMA_VaR(portfolio_returns, alpha = 0.05, distribution = "t-student")
#' }

calculate_EWMA_VaR <- function(portfolio_returns, alpha = 0.05, distribution = "gaussian") {
  # Validate inputs
  if (!is.numeric(portfolio_returns)) {
    stop("portfolio_returns should be a numeric vector.")
  }
  
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha should be a numeric value between 0 and 1.")
  }
  
  # Calculate EWMA volatility
  ewma_volatility <- calculate_EWMA_volatility(portfolio_returns)
  
  # Calculate VaR using the appropriate distribution
  VaR_EWMA <- switch(
    distribution
    , gaussian = qnorm(alpha) * sqrt(tail(ewma_volatility, 1))
    , `t-student` = {
      fit_result = tryCatch({
        suppressWarnings(MASS::fitdistr(portfolio_returns, 't'))
        }, error = function(e) {
          cat('Error in fitting Student\'s t-distribution.\n')
          return(NULL)
        })
      df = unname(fit_result$estimate["df"])
      qt(alpha, df) * sqrt(tail(ewma_volatility, 1))
      }
    , stop("Invalid distribution. Choose 'gaussian' or 't-student'.")
  )
  
  return(VaR_EWMA)
}
