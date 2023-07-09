#' Calculate VaR using fitted GARCH model.
#'
#' This function calculates the Value-at-Risk (VaR) and Expected Shortfall (ES) for a portfolio's returns, 
#' given a fitted GARCH model. It also calculates the number of times the portfolio's actual returns fall 
#' below the calculated VaR.
#'
#' @param portfolio_returns A numeric vector representing the portfolio's returns.
#' @param fit A fitted GARCH model of class 'uGARCHfit'. This can be obtained by using the 'ugarchfit' function from 'rugarch' package.
#' from the 'rugarch' package on your return data.
#' @param alpha A numeric value between 0 and 1 representing the significance level for the VaR and ES. Default is 0.05.
#'
#' @return A list containing the VaR, ES, and the number of times the portfolio's actual returns fall 
#' below the VaR ('Exceeds_Realized').
#'
#' @examples
#' \dontrun{
#' # Generate some return data
#' x = rnorm(1000)
#' # Fit a GARCH(1,1) model to the data
#' spec = rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)))
#' fit = rugarch::ugarchfit(spec, data = x)
#' # Calculate the VaR and ES
#' calculate_VaR_GARCH(x, fit)
#' }

calculate_VaR_GARCH = function(portfolio_returns, fit, alpha = 0.05){
  # Validate inputs
  if (!is.numeric(alpha) || alpha < 0 || alpha > 1) {
    stop("alpha should be a numeric value between 0 and 1.")
  }
  
  if (class(fit) != 'uGARCHfit') {
    stop("fit should fitted model of class uGARCHfit")
  }
  
  volatility = as.numeric(sigma(fit))
  
  VaR = qnorm(alpha) * tail(volatility, 1)
  ES = (-dnorm(qnorm(alpha)) / alpha) * tail(volatility, 1)
  exceeds = sum(portfolio_returns < VaR)
  return(list(VaR = VaR, ES = ES, Exceeds_Realized = exceeds))
}
