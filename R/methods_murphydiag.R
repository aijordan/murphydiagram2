#' Combining Murphy diagram objects
#' 
#' Put 'em together
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
  identical_y <- sapply(inputm,
                        function(m1, m2)
                          isTRUE(all.equal(m1$y, m2$y)),
                        proto)
  identical_func <- sapply(inputm,
                           function(m1, m2)
                             identical(m1$functional, m2$functional),
                           proto)

  if (!all(identical_y))
    stop("incompatible observations")
  if (!all(identical_func))
    stop("incompatible functionals")

  inputx <- lapply(input,
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
#' And split 'em back up
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
#' Plaster them walls
#' 
#' @param x an object inheriting from the class \code{murphydiag}.
#' @inheritParams graphics::matplot
#' 
#' @seealso \code{\link{murphydiag}},
#'   \code{\link{c.murphydiag}},
#'   \code{\link{[.murphydiag}}
#' 
#' @importFrom graphics abline matplot
#' @export
plot.murphydiag <- function(x, type = "l",
                            xlim = NULL,
                            ylim = NULL,
                            xlab = NULL,
                            ylab = NULL, ...) {
  m <- x
  if (is.null(xlab)) xlab <- expression(paste("Parameter ", theta))
  if (is.null(ylab)) ylab <- "mean score"

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
  tt <- c(xlim[1L], sort(unique(tt)), xlim[2L])

  msfun <- ms_fun(m)
  yl <- msfun(tl, right = FALSE)
  yr <- msfun(tr, right = TRUE)

  n <- length(tt)
  interleaved <- rep(1:n, each = 2L) + rep(c(0L, n), n)
  xx <- rep(tt, each = 2L)
  yy <- if (length(m$x) > 1L) {
    rbind(yl, yr)[interleaved, ]
  } else {
    c(yl, yr)[interleaved]
  }

  matplot(xx, yy, type = type, xlim = xlim, ylim = ylim,
          xlab = xlab, ylab = ylab, ...)
  abline(h = 0, lty = 2)

  invisible(m)
}
