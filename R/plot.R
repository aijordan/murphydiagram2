#' Plotting Murphy diagram objects
#' 
#' @param x an object inheriting from the class \code{'murphydiag'}.
#' @param resolution
#'  determines the smoothness (and accuracy) of the displayed lines:
#'  \enumerate{
#'  \item if \code{NULL}: all points returned by \code{as.data.frame(x)} are displayed.
#'  \item if \code{c(n)} or \code{c(nx, ny)}: only points returned by \code{m_filter(x, window, resolution)} with \code{window = par("usr")} are displayed.
#'  \item if \code{c(t1, t2, ..., tn)}: only points returned by \code{m_approx(x, thresholds)} with \code{thresholds = resolution} are displayed.
#'  }
#'  See \code{\link[as.data.frame.murphydiag]{as.data.frame}}, \code{\link{m_filter}}, \code{\link{m_approx}}.
#' @param xlab a label for the x axis, defaults to "threshold".
#' @param ylab a label for the y axis, defaults to "mean score".
#' @param ... other graphical parameters (see \code{\link{par}}).
#' @param add if \code{TRUE}, lines are added to the current plot.
#' @param no_baseline if \code{TRUE}, no horizontal baseline at 0 is drawn.
#' @param empty if \code{TRUE}, no Murphy diagram lines are drawn.
#' @param lty vector of line types, recycled to \code{length(x)}.
#' @param col vector of colors, recycled to \code{length(x)}.
#' @inheritParams graphics::plot.default
#' 
#' @seealso
#'   \code{\link{murphydiag}}.
#'   
#'   To display only a selection of Murphy diagram lines,
#'   consider subsetting (see \code{\link{[.murphydiag}}).
#' 
#' @name plot.murphydiag
NULL

#' @rdname plot.murphydiag
#'
#' @importFrom graphics plot abline
#' 
#' @export
plot.murphydiag <- function(x, resolution = 100,
                            xlim = NULL, ylim = NULL,
                            xlab = NULL, ylab = NULL, ...,
                            add = FALSE, no_baseline = FALSE, empty = FALSE,
                            lty = 1:5, col = 1:6) {
  if (!add) {
    if (is.null(xlab)) xlab <- "threshold"
    if (is.null(ylab)) ylab <- "mean score"
    if (is.null(xlim)) xlim <- range(x, dim = "knots")
    if (is.null(ylim)) ylim <- range(x, dim = "values")
    plot(NULL, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ...)
    if (!no_baseline) abline(h = 0, lty = 2)
  }
  if (!empty) lines(x, resolution, ..., lty = lty, col = col)
  invisible(x)
}

#' @importFrom stats nobs
#'
#' @export
nobs.murphydiag <- function(object, ...) {
  length(attr(object, "y"))
}

#' @rdname plot.murphydiag
#' 
#' @importFrom graphics lines par
#' 
#' @export
lines.murphydiag <- function(x, resolution = 100, ..., lty = 1:5, col = 1:6) {
  k <- length(x)
  if (length(lty) < k) lty <- rep_len(lty, k)
  if (length(col) < k) col <- rep_len(col, k)
  if (is.null(resolution)) {
    for (i in seq_len(k)) {
      df <- as.data.frame(x[i])
      lines(df$knot, df$value, lty = lty[i], col = col[i], ...)
    }
  } else if (length(resolution) %in% 1:2) {
    for (i in seq_len(k)) {
      df <- m_filter(x[i], window = par("usr"), resolution)
      lines(df$knot, df$value, lty = lty[i], col = col[i], ...)
    }
  } else if (length(resolution) > 2L) {
    for (i in seq_len(k)) {
      df <- m_approx(x[i], thresholds = resolution)
      lines(df$threshold, df$value, lty = lty[i], col = col[i], ...)
    }
  }
  
  invisible(x)
}

#' @importFrom ggplot2 autoplot
#' 
#' @export
autoplot.murphydiag <- function(object, resolution = 100, ...) {
  requireNamespace("ggplot2")
  p <- if (is.null(resolution)) {
    df <- as.data.frame(object)
    mapping <- ggplot2::aes(x = knot, y = value, color = name)
    ggplot2::ggplot(data = df, mapping = mapping)
  } else if (length(resolution) %in% 1:2) {
    df <- m_filter(object, range(object), resolution = resolution)
    mapping <- ggplot2::aes(x = knot, y = value, color = name)
    ggplot2::ggplot(data = df, mapping = mapping)
  } else if (length(resolution > 2L)) {
    df <- m_approx(object, thresholds = resolution)
    mapping <- ggplot2::aes(x = threshold, y = value, color = name)
    ggplot2::ggplot(data = df, mapping = mapping)
  }
  p +
    ggplot2::labs(x = "threshold", y = "mean score", color = "model") +
    ggplot2::geom_line(...) +
    ggplot2::geom_hline(yintercept = 0, lty = 2)
}

#' @importFrom ggplot2 autolayer
#' 
#' @export
autolayer.murphydiag <- function(object, resolution = 100, ...) {
  requireNamespace("ggplot2")
  if (is.null(resolution)) {
    df <- as.data.frame(object)
    mapping <- ggplot2::aes(x = knot, y = value, color = name)
  } else if (length(resolution) %in% 1:2) {
    df <- m_filter(object, range(object), resolution = resolution)
    mapping <- ggplot2::aes(x = knot, y = value, color = name)
  } else if (length(resolution > 2L)) {
    df <- m_approx(object, thresholds = resolution)
    mapping <- ggplot2::aes(x = threshold, y = value, color = name)
  }
  ggplot2::geom_line(mapping = mapping, data = df, ...)
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