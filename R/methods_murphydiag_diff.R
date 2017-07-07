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


#' Plotting Murphy diagram difference objects
#' 
#' Plaster them walls
#' 
#' @param x an object inheriting from the class \code{murphydiag}.
#' @param main an overall title for the plot: see \code{\link{title}}.
#' @param level_ci level for confidence interval
#' @param sd_lags number of autocovariances to use for confidence intervals (defaults to zero; values 1, 2, 3, ... may make sense for time series data. Set to \code{NULL} for data-driven selection.)
#' @inheritParams graphics::matplot
#' 
#' @seealso \code{\link{murphydiag_diff}},
#'   \code{\link{-.murphydiag}},
#'   \code{\link{[.murphydiag}}
#' 
#' @importFrom graphics abline matplot lines polygon
#' @importFrom stats qnorm sd lm
#' @importFrom magrittr "%>%"
#' @export
plot.murphydiag_diff <- function(x, type = "l",
                                 xlim = NULL,
                                 ylim = NULL,
                                 main = NULL,
                                 xlab = NULL,
                                 ylab = NULL, 
                                 level_ci = 0.95, 
                                 sd_lags = 0,
                                 ...) {
  m <- x
  class(m) <- "murphydiag"
  if (is.null(main)) main <- sprintf("%s vs %s", names(m$x)[1L], names(m$x)[2L])
  if (is.null(xlab)) xlab <- expression(paste("Parameter ", theta))
  if (is.null(ylab)) ylab <- "mean score difference"
  
  tt <- c(m$x, m$y, recursive = TRUE, use.names = FALSE)
  if (is.null(xlim)) {
    if (identical(m$functional$type, "prob")) {
      xlim <- c(0, 1)
    } else {
      range_tt <- range(tt)
      xlim <- range_tt + c(-.05, .05) * (range_tt[2L] - range_tt[1L])
    }
  }
  tt <- tt[tt > xlim[1L] & tt < xlim[2L]]
  tt <- sort(unique(tt))
  
  fsfun <- fs_fun(m)
  tl <- c(xlim[1L], tt)
  tr <- c(tt, xlim[2L])
  yl_main <- fsfun(tl, right = FALSE)
  yr_main <- fsfun(tr, right = TRUE)
  
  yl <- yl_main %>% sapply(colMeans) %>% t %>% (function(z) z[, 1L] - z[, 2L])
  yl_sd <- yl_main %>% lapply(function(z) z[, 1L] - z[, 2L]) %>% 
    sapply(hac_sd, k = sd_lags)
  yr <- yr_main %>% sapply(colMeans) %>% t %>% (function(z) z[, 1L] - z[, 2L])
  yr_sd <- yr_main %>% lapply(function(z) z[, 1L] - z[, 2L]) %>% 
    sapply(hac_sd, k = sd_lags)
  
  n <- length(tt) + 1L
  interleaved <- rep(1:n, each = 2L) + rep(c(0L, n), n)
  xx <- c(tl, tr)[interleaved]
  yy <- c(yl, yr)[interleaved]
  
  if (is.null(level_ci)){
    
    matplot(xx, yy, type = type, xlim = xlim, ylim = ylim,
            main = main, xlab = xlab, ylab = ylab, ...)
    abline(h = 0, lty = 2)
    
    } else {
      
    a_lb <- 0.5*(1-level_ci)
    yy_lb <- c(yl + qnorm(a_lb)*yl_sd, yr + qnorm(a_lb)*yr_sd)[interleaved]
    a_ub <- 0.5 + 0.5*level_ci
    yy_ub <- c(yl + qnorm(a_ub)*yl_sd, yr + qnorm(a_ub)*yr_sd)[interleaved]
        
    if (is.null(ylim)){
      ylim <- range(yy_lb, yy_ub)
    }  
        
    matplot(xx, yy, type = "n", xlim = xlim, ylim = ylim,
            main = main, xlab = xlab, ylab = ylab, ...)
    polygon(c(xx, rev(xx)), c(yy_ub, rev(yy_lb)), col = "grey", 
            border = NA)
    lines(xx, yy, type = type)
    abline(h = 0, lty = 2)
      
  }
  
  invisible(m)
}