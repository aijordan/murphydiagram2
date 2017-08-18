#' Elementary score functions
#' 
#' Functions for the computation of the elementary scores.
#' 
#' @param x an object convertible by \code{as.matrix}
#'   containing forecasts (dim: #observations,
#'   #forecast methods).
#' @param y an object convertible by \code{as.vector}
#'   containing observations.
#' @param t a single threshold value.
#' @param level a single value in (0, 1).
#' @param right logical, indicating if the scores are
#'   right-continuous in \code{x}.
#' 
#' @return yadda
#' 
#' @name elementaryscores
NULL


#' @rdname elementaryscores
#' @export
es_expect <- function(a, t, x, y, right = FALSE) {
  a <- as.vector(a, "numeric")
  a[a <= 0 | a >= 1] <- NaN
  t <- as.vector(t, "numeric")
  x <- as.matrix(x)
  y <- as.vector(y, "numeric")
  right <- as.vector(right, "logical")
  
  na <- length(a)
  nt <- length(t)
  nx <- dim(x)[2L]
  ny <- length(y)
  stopifnot(identical(length(right), 1L))
  if (!identical(dim(x)[1L], ny))
    stop("forecast matrix is incompatible with observation vector")
  
  outdim <- c(ny, nx, nt, na)
  if (!identical(na, 1L)) a <- rep(a, each = ny * nx * nt)
  if (!identical(nt, 1L)) t <- rep(t, each = ny * nx)
  dim(x) <- NULL
  
  tx <- if (right) t <= x else t < x
  ty <- if (right) t <= y else t < y
  I <- ((y < x) - a) * abs(t - y)
  
  array(2 * I * (tx - ty), outdim)
}

#' @rdname elementaryscores
#' @export es_mean
es_mean <- function(t, x, y, right = FALSE)
  2 * es_expect(t, x, y, 0.5, right)

#' @rdname elementaryscores
#' @export es_median
es_median <- function(t, x, y, right = FALSE)
  2 * es_quant(t, x, y, 0.5, right)

#' @rdname elementaryscores
#' @export es_prob
es_prob <- function(t, x, y, right = FALSE) {
  x <- as.matrix(x)
  y <- as.vector(y)
  x[x < 0 | x > 1] <- NaN
  y[y != 0 & y != 1] <- NaN
  
  2 * es_expect(t, x, y, 0.5, right)
}

#' @rdname elementaryscores
#' @export es_quant
es_quant <- function(t, x, y, level, right = FALSE) {
  stopifnot(identical(length(t), 1L))
  stopifnot(identical(length(level), 1L))
  stopifnot(identical(length(right), 1L))
  
  x <- as.matrix(x)
  y <- as.vector(y)
  level[!(level > 0 && level < 1)] <- NaN
  
  if (!identical(dim(x)[1L], length(y)))
    stop("forecast matrix is incompatible with observation vector")
  
  tx <- if (right) t <= x else t < x
  ty <- if (right) t <= y else t < y
  I <- (y < x) - level
  drop(I * (tx - ty))
}
