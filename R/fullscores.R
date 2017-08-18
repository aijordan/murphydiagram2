#' @rdname scores
#' @export fs_mean
fs_mean <- function(t, x, y, right = FALSE)
  lapply(fs_expect(t, x, y, 0.5, right), function(z) 2*z)


#' @rdname scores
#' @export fs_prob
fs_prob <- function(t, x, y, right = FALSE) {
  x <- as.matrix(x)
  y <- as.vector(y)
  x[x < 0 | x > 1] <- NaN
  y[y != 0 & y != 1] <- NaN
  
  lapply(fs_expect(t, x, y, 0.5, right), function(z) 2*z)
}


#' @rdname scores
#' @export fs_median
fs_median <- function(t, x, y, right = FALSE)
  lapply(fs_quant(t, x, y, 0.5, right), function(z) 2*z)


#' @rdname scores
#' @export fs_quant
fs_quant <- function(t, x, y, level, right = FALSE) {
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
    (I * (tx - ty))
  })
  rval
}


#' @rdname scores
#' @export fs_expect
fs_expect <- function(t, x, y, level, right = FALSE) {
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
    (I * (tx - ty))
  })
  rval
}
