#' Plotting Murphy diagram difference objects
#' 
#' Plaster them walls
#' 
#' @param m an object inheriting from the class \code{murphydiag}.
#' @inheritParams graphics::matplot
#' 
#' @seealso \code{\link{murphydiag_diff}},
#'   \code{\link{-.murphydiag}},
#'   \code{\link{[.murphydiag}}
#' 
#' @export plot.murphydiag_diff
plot.murphydiag_diff <- function(m, type = "l",
                                 xlim = NULL,
                                 ylim = NULL,
                                 main = NULL,
                                 xlab = NULL,
                                 ylab = NULL, ...) {
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
  
  msfun <- ms_fun(m)
  tl <- c(xlim[1L], tt)
  tr <- c(tt, xlim[2L])
  yl <- msfun(tl, right = FALSE)
  yl <- yl[, 1L] - yl[, 2L]
  yr <- msfun(tr, right = TRUE)
  yr <- yr[, 1L] - yr[, 2L]
  
  n <- length(tt) + 1L
  interleaved <- rep(1:n, each = 2L) + rep(c(0L, n), n)
  xx <- c(tl, tr)[interleaved]
  yy <- c(yl, yr)[interleaved]
  
  matplot(xx, yy, type = type, xlim = xlim, ylim = ylim,
          main = main, xlab = xlab, ylab = ylab, ...)
  abline(h = 0, lty = 2)
  
  invisible(m)
}