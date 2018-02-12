#' Elementary score functions
#' 
#' Functions for the computation of the elementary scores.
#' 
#' @param t an object convertible by \code{as.vector}
#'  containing treshold values.
#' @param x an object convertible by \code{as.matrix}
#'   containing forecasts (dim: #observations,
#'   #forecast methods).
#' @param y an object convertible by \code{as.vector}
#'   containing observations.
#' @param level numerical, a single value between 0 and 1.
#' @param right logical, indicating if the scores are
#'   right-continuous in \code{t}.
#' 
#' @return Returns an array with dimension
#'  (#thresholds, #observations, #forecast methods).
#' 
#' @name elementaryscores
NULL

es_genquant <- function(expr, t, x, y, level, right) {
  t <- as.vector(t, "numeric")
  x <- as.matrix(x)
  y <- as.vector(y, "numeric")
  level <- as.vector(level, "numeric")
  right <- as.vector(right, "logical")
  
  stopifnot(identical(dim(x)[1L], length(y)))
  stopifnot(identical(length(level), 1L))
  stopifnot(level > 0, level < 1)
  stopifnot(identical(length(right), 1L))
  
  dout <- c(length(t), dim(x))
  dim(x) <- NULL
  if (!identical(dout[1], 1L)) {
    x <- rep(x, rep.int(dout[1], length(x)))
    y <- rep(y, rep.int(dout[1], length(y)))
  }
  xlt <- if (right) (x <= t) else (x < t)
  ylt <- if (right) (y <= t) else (y < t)
  array(eval(expr), dout)
}


#' @rdname elementaryscores
#' @export
es_expect <- function(t, x, y, level, right = FALSE) {
  expr <- expression(abs(4 * (ylt - level) * (t - y) * (ylt - xlt)))
  es_genquant(expr, t, x, y, level, right)
}


#' @rdname elementaryscores
#' @export es_mean
es_mean <- function(t, x, y, right = FALSE) {
  expr <- expression(2 * (t - y) * (ylt - xlt))
  es_genquant(expr, t, x, y, 0.5, right)
}


#' @rdname elementaryscores
#' @export es_median
es_median <- function(t, x, y, right = FALSE) {
  expr <- expression(abs(ylt - xlt))
  es_genquant(expr, t, x, y, 0.5, right)
}


#' @rdname elementaryscores
#' @export es_prob
es_prob <- function(t, x, y, right = FALSE) {
  x <- as.matrix(x)
  x[x < 0 | x > 1] <- NaN
  y <- as.vector(y, "numeric")
  y[y != 0 & y != 1] <- NaN
  es_mean(t, x, y, right)
}


#' @rdname elementaryscores
#' @export es_quant
es_quant <- function(t, x, y, level, right = FALSE) {
  expr <- expression(2 * (ylt - level) * (ylt - xlt))
  es_genquant(expr, t, x, y, level, right)
}
