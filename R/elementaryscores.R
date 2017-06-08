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
#' @export es_mean
es_mean <- function(t, x, y, right = FALSE)
  2 * es_expect(t, x, y, 0.5, right)


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
#' @export es_median
es_median <- function(t, x, y, right = FALSE)
  2 * es_quant(t, x, y, 0.5, right)


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


#' @rdname elementaryscores
#' @export es_expect
es_expect <- function(t, x, y, level, right = FALSE) {
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
  I <- ((y < x) - level) * abs(t - y)
  drop(I * (tx - ty))
}
