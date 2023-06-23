#' Calculate Value at Risk (VaR) and Expected Shortfall (ES) of a portfolio
#'
#' This function calculates the Value at Risk (VaR) and Expected Shortfall (ES) using
#' one of the three methods: historical, gaussian (parametric), and t-student.
#'
#' @param portfolio_returns A numeric vector of portfolio returns.
#' @param alpha A numeric value between 0 and 1 representing the significance level.
#' @param method A character string specifying the method to be used ('historical', 'gaussian', 't-student' or 'ewma'). Default is 'historical'.
#' @param distribution If method = 'ewma', then a character string specifying which distribution to use for calculating VaR ('gaussian' or 't-student').
#'
#' @return A list containing the VaR, ES, Realized Exceedances and Expected Exceedances.
#' @export
#'
#' @examples
#' # Example with historical method
#' returns <- rnorm(1000, mean=0.0005, sd=0.01)
#' calculate_VaR(returns, alpha=0.05, method="historical")
#'
#' # Example with gaussian method
#' calculate_VaR(returns, alpha=0.05, method="gaussian")
#'
#' # Example with t-student method
#' calculate_VaR(returns, alpha=0.05, method="t-student")

calculate_VaR <- function(portfolio_returns, alpha = 0.05, method = "historical", distribution = NULL) {
  # Input validation
  if (!is.numeric(portfolio_returns)) {
    stop("portfolio_returns should be a numeric vector.")
  }
  
  if (length(portfolio_returns) < 2) {
    stop("portfolio_returns should contain at least two data points.")
  }
  
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha should be a numeric value between 0 and 1.")
  }
  
  if (!is.character(method) || !method %in% c("historical", "gaussian", "t-student", "ewma")) {
    stop("Invalid method. Choose 'historical', 'gaussian', 't-student' or 'ewma'.")
  }
  
  if (method == "ewma" && !distribution %in% c("gaussian", "t-student")) {
    stop("Invalid distribution. Choose 'gaussian' or 't-student'.")
  }
  
  n = length(portfolio_returns)
  
  # Call the appropriate calculation function
  results <- switch(
    method
    , historical = calculate_VaR_historical(portfolio_returns, alpha)
    , gaussian = calculate_VaR_gaussian(portfolio_returns, alpha)
    , `t-student` = calculate_VaR_t_student(portfolio_returns, alpha)
    , ewma = calculate_VaR_EWMA(portfolio_returns, alpha, distribution)
    , stop("Invalid method. Choose 'historical', 'gaussian', 't-student' or 'ewma'.")
  )
  
  # Add expected exceeds to results
  results$Exceeds_Expected = alpha * n
  
  # Return results
  return(results)
}
