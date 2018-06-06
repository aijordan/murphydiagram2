# Function to compute variance estimator which accounts for autocorrelation
hac_sd <- function(x, k = 0){
  
  u <- x - mean(x)
  n <- length(u)
  
  # choose nr of lags (if not provided)
  if (is.null(k)){
    
    aux <- lm(u[-1]~u[-n]-1)
    rho <- unname(aux$coefficients)
    sigma <- sum(unname(aux$residuals)^2) / n
    
    top <- sum( (4*(rho^2) * (sigma^2)) / (((1-rho)^6)*((1+rho)^2)) )
    bot <- sum( (sigma^2) / ((1-rho)^4) )
    k <- min(c(ceiling(1.1447*((top/bot)*n)^(1/3)), round(0.5*n)))
    
  }
  
  # compute HAC
  vcv <- sum(u^2) / n
  
  # If k > 0: Add autocovariance terms
  if (k > 0){
    w <- 1 - (1:k)/(k+1)
    for (i in 1:k){
      cov <- sum(u[(i+1):n] * u[1:(n-i)]) / n
      vcv <- vcv + 2*w[i]*cov
    }
  }
  
  sqrt(vcv/n)
  
}
