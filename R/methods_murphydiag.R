#' Combining Murphy diagram objects
#' 
#' Combine two or more 'murphydiag' objects that are based on the same observations and type of forecasts.
#' 
#' @param ... objects to be concatenated.
#' 
#' @return An object inheriting from the \code{murphydiag} class.
#' 
#' @seealso \code{\link{[.murphydiag}}, \code{\link{murphydiag}}
#' 
#' @export
c.murphydiag <- function(...) {
  input <- list(...)
  input <- input[!sapply(input, is.null)]

  proto <- input[[1L]]
  inputm <- input[sapply(input, inherits, "murphydiag")]
  identical_y <-
    sapply(inputm,
           function(m1, m2) {
             isTRUE(all.equal(m1$y, m2$y))
           },
           proto)
  identical_func <-
    sapply(inputm,
           function(m1, m2) {
             identical(m1$functional, m2$functional)
           },
           proto)
  if (!all(identical_y))
    stop("incompatible observations")
  if (!all(identical_func))
    stop("incompatible functionals")

  inputx <-
    lapply(input,
           function(m) {
             if (inherits(m, "murphydiag"))
               m$x
             else
               m
           })
  
  rval <- list(
    x = as.data.frame(inputx),
    y = proto$y,
    functional = proto$functional)

  stopifnot(all(sapply(rval$x, is.numeric)),
            identical(dim(rval$x)[1L], length(rval$y)))

  class(rval) <- "murphydiag"
  rval
}

#' Subsetting Murphy diagram objects
#' 
#' @param m an object inheriting from the class \code{murphydiag}.
#' @param j index specifying elements to extract.
#' 
#' @return (see \link{[.data.frame}).
#' 
#' @seealso \code{\link{c.murphydiag}}
#' 
#' @export
`[.murphydiag` <- function(m, j) {
  m$x <- m$x[, j, drop = FALSE]
  m
}


#' Plotting Murphy diagram objects
#' 
#' @param x an object inheriting from the class \code{murphydiag}.
#' @param thresholds either the maximum number of thresholds (if length 1) or the values against which to plot 
#' @inheritParams graphics::matplot
#' 
#' @seealso \code{\link{murphydiag}},
#'   \code{\link{c.murphydiag}},
#'   \code{\link{[.murphydiag}}
#' 
#' @importFrom graphics abline matplot
#' @export
plot.murphydiag <- function(x, thresholds = 500L,
                            type = "l",
                            xlim = NULL, ylim = NULL,
                            xlab = NULL, ylab = NULL, ...) {
  m <- x
  if (is.null(xlab)) xlab <- "threshold"
  if (is.null(ylab)) ylab <- "mean score"

  if (is.null(xlim)) {
    if (identical(m$functional$type, "prob")) {
      xlim <- c(0, 1)
    } else {
      range_tt <- range(m$x, m$y)
      xlim <- range_tt + c(-.05, .05) * (range_tt[2L] - range_tt[1L])
    }
  }
  
  interp <- identical(length(thresholds), 1L) && thresholds < length(m$y)
  
  msfun <- ms_fun(m)
  if (interp) {
    stopifnot(thresholds > 2L)
    xx <- seq(xlim[1L], xlim[2L], length.out = thresholds)
    mid <- floor(0.5 * thresholds)
    y1 <- msfun(xx[1:mid], right = FALSE)
    y2 <- msfun(xx[(mid + 1):thresholds], right = TRUE)
    yy <- if (length(m$x) > 1L) rbind(y1, y2) else c(y1, y2)
  } else {
    tt <- if (length(thresholds) > 1L) {
      thresholds
    } else {
      c(m$x, m$y, recursive = TRUE, use.names = FALSE)
    }
    tt <- tt[tt > xlim[1L] & tt < xlim[2L]]
    tt <- c(xlim[1L], sort(unique(tt)), xlim[2L])
    yl <- msfun(tt, right = FALSE)
    yr <- msfun(tt, right = TRUE)
    n <- length(tt)
    interleaved <- rep(1:n, each = 2L) + rep(c(0L, n), n)
    xx <- rep(tt, each = 2L)
    yy <- if (length(m$x) > 1L) {
      rbind(yl, yr)[interleaved, ]
    } else {
      c(yl, yr)[interleaved]
    }
  }

  matplot(xx, yy, type = type, xlim = xlim, ylim = ylim,
          xlab = xlab, ylab = ylab, ...)
  abline(h = 0, lty = 2)

  invisible(m)
}

#' Converting a murphydiag object into a data.frame
#' 
#' @inheritParams plot.murphydiag
#' 
#' @return A \code{data.frame} with three columns: \code{method}, \code{threshold}, \code{score}
#'
#' @export
as.data.frame.murphydiag <- function(x, ..., thresholds = 500L) {
  m <- x
  if (identical(m$functional$type, "prob")) {
    range_tt <- c(0, 1)
    xlim <- range_tt
  } else {
    range_tt <- range(m$x, m$y)
    xlim <- range_tt + c(-.05, .05) * (range_tt[2L] - range_tt[1L])
  }
  
  interp <- identical(length(thresholds), 1L) && thresholds < length(m$y)
  
  msfun <- ms_fun(m)
  if (interp) {
    stopifnot(thresholds > 2L)
    xx <- seq(xlim[1L], xlim[2L], length.out = thresholds)
    mid <- floor(0.5 * thresholds)
    y1 <- msfun(xx[1:mid], right = FALSE)
    y2 <- msfun(xx[(mid + 1):thresholds], right = TRUE)
    yy <- if (length(m$x) > 1L) rbind(y1, y2) else c(y1, y2)
    
    data.frame(
      method = rep(names(m$x), rep.int(length(xx), length(m$x))),
      threshold = xx,
      score = c(yy))
    
  } else {
    if (length(thresholds) > 1L) {
      tt <- sort(unique(thresholds))
      yl <- msfun(tt, right = FALSE)
      yr <- msfun(tt, right = TRUE)
      n <- length(tt)
      interleaved <- rep(1:n, each = 2L) + rep(c(0L, n), n)
      xx <- rep(tt, each = 2L)
      yy <- if (length(m$x) > 1L) {
        rbind(yl, yr)[interleaved, ]
      } else {
        c(yl, yr)[interleaved]
      }
      
      data.frame(
        method = rep(names(m$x), rep.int(length(xx), length(m$x))),
        threshold = xx,
        score = c(yy))
      
    } else {
      do.call(rbind,
              lapply(seq_along(m$x), function(i) {
                as.data.frame(m[i], thresholds = c(m$x[[i]], m$y, range_tt))
              })
      )
    }
  }
}