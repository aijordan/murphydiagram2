#' Plotting Murphy diagram objects
#' 
#' @param x an object inheriting from the class \code{murphydiag}.
#' @param thresholds either the maximum number of thresholds (if length 1) or the values against which to plot 
#' @param right.cont logical; whether the diagrams are right-continuous or not. Alternatively: the string "both"
#' @inheritParams graphics::matplot
#' 
#' @seealso \code{\link{murphydiag}},
#'   \code{\link{c.murphydiag}},
#'   \code{\link{[.murphydiag}}
#' 
#' @importFrom graphics abline matplot
#' @export
plot.murphydiag <- function(x, thresholds = 1e4L, right.cont = "both",
                            type = "l",
                            xlim = NULL, ylim = NULL,
                            xlab = NULL, ylab = NULL, ...) {
  m <- x
  if (is.null(xlab)) xlab <- "threshold"
  if (is.null(ylab)) ylab <- "mean score"
  if (is.null(xlim)) {
    if (identical(attr(m, "functional")$type, "prob")) {
      xlim <- c(0, 1)
    } else {
      range_tt <- range(
        attr(m, "y"),
        sapply(m, function(d) range(d$x))
      )
      xlim <- range_tt + c(-.05, .05) * (range_tt[2L] - range_tt[1L])
    }
  }
  if (is.null(ylim)) {
    ylim <- c(0, max(sapply(m, function(d) d$values$max)))
  }
  
  # if (length(thresholds) == 1 && thresholds < length(attr(m, "y"))) {
  #   stopifnot(thresholds > 2L)
  #   tt <- seq(xlim[1L], xlim[2L], length.out = thresholds)
  # } else if (length(thresholds) > 1L) {
  #   tt <- thresholds
  #   tt <- tt[tt > xlim[1L] & tt < xlim[2L]]
  #   tt <- c(xlim[1L], sort(unique(tt)), xlim[2L])
  # } else {
  #   tt <- c(m$x, m$y, recursive = TRUE, use.names = FALSE)
  #   tt <- tt[tt > xlim[1L] & tt < xlim[2L]]
  #   tt <- c(xlim[1L], sort(unique(tt)), xlim[2L])
  # }
  
  # msfun <- ms_fun(m)
  # xx <- tt
  # yy <- msfun(xx, right.cont)
  # if (right.cont == "both") xx <- rep(tt, each = 2L)
  plot(NULL, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ...)
  for (i in seq_along(m)) {
    lines(rep(m[[i]]$knots, each = 2L), do.call(rbind, m[[i]]$values[1:2]))
  }
  # matplot(xx, yy, type = type, xlim = xlim, ylim = ylim,
  #         xlab = xlab, ylab = ylab, ...)
  abline(h = 0, lty = 2)
  
  invisible(m)
}


#' Plotting Murphy diagram difference objects
#' 
#' @param x an object inheriting from the class \code{murphydiag_diff}.
#' @inheritParams plot.murphydiag
#' @param main an overall title for the plot: see \code{\link{title}}.
#' @param level_ci level for confidence interval (set to \code{NULL} to omit it)
#' @param sd_lags number of autocovariances to use for confidence intervals
#'   (defaults to zero; values 1, 2, 3, ... may make sense for time series data.
#'    Set to \code{NULL} for data-driven selection.)
#' @inheritParams graphics::matplot
#' 
#' @seealso \code{\link{murphydiag_diff}},
#'   \code{\link{-.murphydiag}},
#'   \code{\link{[.murphydiag}}
#' 
#' @importFrom graphics abline matplot lines polygon
#' @importFrom stats qnorm sd lm
#' @export
plot.murphydiag_diff <- function(x, ...,
                                 thresholds = 500L,
                                 type = "l",
                                 xlim = NULL, ylim = NULL,
                                 main = NULL,
                                 xlab = NULL, ylab = NULL, 
                                 level_ci = 0.95, 
                                 sd_lags = 0) {
  
  stopifnot(is.null(level_ci) | (level_ci > 0 & level_ci < 1), 
            is.null(sd_lags) | (sd_lags %in% 0:length(x)))
  
  m <- x
  class(m) <- "murphydiag"
  if (is.null(main)) main <- sprintf("%s vs %s", names(m$x)[1L], names(m$x)[2L])
  if (is.null(xlab)) xlab <- "threshold"
  if (is.null(ylab)) ylab <- "mean score difference"
  
  if (is.null(xlim)) {
    if (identical(m$functional$type, "prob")) {
      xlim <- c(0, 1)
    } else {
      range_tt <- range(m$x, m$y)
      xlim <- range_tt + c(-.05, .05) * (range_tt[2L] - range_tt[1L])
    }
  }
  
  interp <- identical(length(thresholds), 1L) && thresholds < length(m$y)
  
  esfun <- es_fun(m)
  if (interp) {
    stopifnot(thresholds > 2L)
    xx <- seq(xlim[1L], xlim[2L], length.out = thresholds)
    mid <- floor(0.5 * thresholds)
    y1 <- esfun(xx[1:mid], right = FALSE)
    y2 <- esfun(xx[(mid + 1):thresholds], right = TRUE)
    y1 <- apply(y1, 1, function(z) z[, 1L] - z[, 2L])
    y2 <- apply(y2, 1, function(z) z[, 1L] - z[, 2L])
    yy <- c(colMeans(y1), colMeans(y2))
    if (is.null(level_ci)) {
      yy_lb <- NULL
      yy_ub <- NULL
    } else {
      yy_sd <- c(apply(y1, 2, hac_sd, k = sd_lags),
                 apply(y2, 2, hac_sd, k = sd_lags))
      a_lb <- 0.5 * (1 - level_ci)
      a_ub <- 0.5 * (1 + level_ci)
      yy_lb <- yy + qnorm(a_lb) * yy_sd
      yy_ub <- yy + qnorm(a_ub) * yy_sd
    }
  } else {
    tt <- if (length(thresholds) > 1L) {
      thresholds
    } else {
      c(m$x, m$y, recursive = TRUE, use.names = FALSE)
    }
    tt <- tt[tt > xlim[1L] & tt < xlim[2L]]
    tt <- c(xlim[1L], sort(unique(tt)), xlim[2L])
    yl_main <- esfun(tt, right = FALSE)
    yr_main <- esfun(tt, right = TRUE)
    yl_main <- apply(yl_main, 1, function(z) z[, 1L] - z[, 2L])
    yr_main <- apply(yr_main, 1, function(z) z[, 1L] - z[, 2L])
    
    yl <- colMeans(yl_main)
    yr <- colMeans(yr_main)
    
    
    n <- length(tt)
    interleaved <- rep(1:n, each = 2L) + rep(c(0L, n), n)
    xx <- rep(tt, each = 2L)
    yy <- c(yl, yr)[interleaved]
    if (is.null(level_ci)) {
      yy_lb <- NULL
      yy_ub <- NULL
    } else {
      yl_sd <- apply(yl_main, 2, hac_sd, k = sd_lags)
      yr_sd <- apply(yr_main, 2, hac_sd, k = sd_lags)
      a_lb <- 0.5 * (1 - level_ci)
      a_ub <- 0.5 * (1 + level_ci)
      yy_lb <- c(yl + qnorm(a_lb) * yl_sd, yr + qnorm(a_lb) * yr_sd)[interleaved]
      yy_ub <- c(yl + qnorm(a_ub) * yl_sd, yr + qnorm(a_ub) * yr_sd)[interleaved]
    }
  }
  
  if (is.null(ylim) & !is.null(level_ci))
    ylim <- range(yy_lb, yy_ub)
  
  matplot(xx, yy, type = "n",
          xlim = xlim, ylim = ylim,
          main = main,
          xlab = xlab, ylab = ylab, ...)
  polygon(c(xx, rev(xx)), c(yy_ub, rev(yy_lb)), col = "grey", border = NA)
  lines(xx, yy, type = type)
  abline(h = 0, lty = 2)
  
  invisible(m)
}


#' Plot dominance objects
#'
#' @param x an object inheriting from the class \code{dominance}.
#' @param select choose a subset of forecasting methods.
#' @param origin choose a set of origins.
#' @param ... additional parameters passed to \code{hasseDiagram::hasse}.
#' 
#' @export
plot.dominance <- function(x, ..., select = NULL, origin = NULL) {
  if (!is.null(select)) x <- x[select]
  class(x) <- NULL
  if (!is.null(origin)) {
    ind <- sapply(origin, function(o) x[o, ] | x[, o])
    if (isTRUE(ncol(ind) > 1)) {
      ind <- apply(ind, 1, any)
    }
    x <- x[ind, ind, drop = FALSE]
  }
  hasseDiagram::hasse(x, ...)
}