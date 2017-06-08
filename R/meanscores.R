#' Mean score functions
#' 
#' Functions for the computation of mean elementary scores.
#' 
#' @param t an object convertible by \code{as.vector}
#'   containing threshold values.
#' @param x an object convertible by \code{as.matrix}
#'   containing forecasts (dim: #observations, #forecast methods).
#' @param y an object convertible by \code{as.vector}
#'   containing observations.
#' @param level a single value in (0, 1).
#' @param right logical, indicating if the scores are
#'   right-continuous in \code{x}, hence
#'   left-continuous in \code{t}.
#'   
#' @return hubba
#' 
#' @name meanscores
NULL


#' @rdname meanscores
#' @export ms_mean
ms_mean <- function(t, x, y, right = FALSE)
  2 * ms_expect(t, x, y, 0.5, right)


#' @rdname meanscores
#' @export ms_prob
ms_prob <- function(t, x, y, right = FALSE) {
  x <- as.matrix(x)
  y <- as.vector(y)
  x[x < 0 | x > 1] <- NaN
  y[y != 0 & y != 1] <- NaN
  
  2 * ms_expect(t, x, y, 0.5, right)
}
  

#' @rdname meanscores
#' @export ms_median
ms_median <- function(t, x, y, right = FALSE)
  2 * ms_quant(t, x, y, 0.5, right)


#' @rdname meanscores
#' @export ms_quant
ms_quant <- function(t, x, y, level, right = FALSE) {
  stopifnot(identical(length(level), 1L))
  stopifnot(identical(length(right), 1L))
  
  t <- as.vector(t)
  x <- as.matrix(x)
  y <- as.vector(y)
  level[!(level > 0 && level < 1)] <- NaN
  
  if (!identical(dim(x)[1L], length(y)))
    stop("forecast object is incompatible with observation vector")
  
  rval <- lapply(t, function(t) {
    tx <- if (right) t <= x else t < x
    ty <- if (right) t <= y else t < y
    I <- (y < x) - level
    colMeans(I * (tx - ty))
  })
  drop(do.call(rbind, rval))
}


#' @rdname meanscores
#' @export ms_expect
ms_expect <- function(t, x, y, level, right = FALSE) {
  stopifnot(identical(length(level), 1L))
  stopifnot(identical(length(right), 1L))
  
  t <- as.vector(t)
  x <- as.matrix(x)
  y <- as.vector(y)
  level[!(level > 0 && level < 1)] <- NaN
  
  if (!identical(dim(x)[1L], length(y)))
    stop("forecast object is incompatible with observation vector")

  rval <- lapply(as.vector(t), function(t) {
    tx <- if (right) t <= x else t < x
    ty <- if (right) t <= y else t < y
    I <- ((y < x) - level) * abs(t - y)
    colMeans(I * (tx - ty))
  })
  drop(do.call(rbind, rval))
}
