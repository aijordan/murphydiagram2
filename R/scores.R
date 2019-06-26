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


#' Mean score functions
#' 
#' Functions for the computation of mean elementary scores.
#'
#' @inheritParams elementaryscores
#'   
#' @return A matrix of average scores of dimension (length of \code{t}, number of forecast methods).
#' 
#' @name meanscores
NULL

#' @rdname meanscores
#' @export ms_expect
ms_expect <- function(t, x, y, level, right = FALSE)
  apply(es_expect(t, x, y, level, right), c(1, 3), mean)

#' @rdname meanscores
#' @export ms_mean
ms_mean <- function(t, x, y, right = FALSE)
  apply(es_mean(t, x, y, right), c(1, 3), mean)

#' @rdname meanscores
#' @export ms_median
ms_median <- function(t, x, y, right = FALSE)
  apply(es_median(t, x, y, right), c(1, 3), mean)

#' @rdname meanscores
#' @export ms_prob
ms_prob <- function(t, x, y, right = FALSE)
  apply(es_prob(t, x, y, right), c(1, 3), mean)

#' @rdname meanscores
#' @export ms_quant
ms_quant <- function(t, x, y, level, right = FALSE)
  apply(es_quant(t, x, y, level, right), c(1, 3), mean)


#' Elementary score function constructors
#' 
#' Creating functions for the computation of mean elementary scores.
#' 
#' @inheritParams elementaryscores
#'   
#' @return A function in two arguments: 
#'   \tabular{ll}{
#'   \code{t} \tab an object convertible by \code{as.vector}
#'  containing treshold values.\cr
#'   \code{right} \tab logical, indicating if the scores are
#'   right-continuous in \code{t}.
#'   }
#' 
#' @name elementaryscores_fun
NULL

#' @rdname elementaryscores_fun
#' @export es_expect_fun
es_expect_fun <- function(x, y, level)
  function(t, right = FALSE) es_expect(t, x, y, level, right)

#' @rdname elementaryscores_fun
#' @export es_mean_fun
es_mean_fun <- function(x, y)
  function(t, right = FALSE) es_mean(t, x, y, right)

#' @rdname elementaryscores_fun
#' @export es_median_fun
es_median_fun <- function(x, y)
  function(t, right = FALSE) es_median(t, x, y, right)

#' @rdname elementaryscores_fun
#' @export es_prob_fun
es_prob_fun <- function(x, y)
  function(t, right = FALSE) es_prob(t, x, y, right)

#' @rdname elementaryscores_fun
#' @export es_quant_fun
es_quant_fun <- function(x, y, level)
  function(t, right = FALSE) es_quant(t, x, y, level, right)


#' Mean score function constructors
#' 
#' Creating functions for the computation of mean elementary scores.
#' 
#' @inheritParams elementaryscores
#'   
#' @return A function in two arguments: 
#'   \tabular{ll}{
#'   \code{t} \tab an object convertible by \code{as.vector}
#'  containing treshold values.\cr
#'   \code{right} \tab logical, indicating if the scores are
#'   right-continuous in \code{t}.
#'   }
#' 
#' @name meanscores_fun
NULL

#' @rdname meanscores_fun
#' @export ms_mean_fun
ms_mean_fun <- function(x, y)
  function(t, right = FALSE) ms_mean(t, x, y, right)

#' @rdname meanscores_fun
#' @export ms_prob_fun
ms_prob_fun <- function(x, y)
  function(t, right = FALSE) ms_prob(t, x, y, right)

#' @rdname meanscores_fun
#' @export ms_median_fun
ms_median_fun <- function(x, y)
  function(t, right = FALSE) ms_median(t, x, y, right)

#' @rdname meanscores_fun
#' @export ms_quant_fun
ms_quant_fun <- function(x, y, level)
  function(t, right = FALSE) ms_quant(t, x, y, level, right)

#' @rdname meanscores_fun
#' @export ms_expect_fun
ms_expect_fun <- function(x, y, level)
  function(t, right = FALSE) ms_expect(t, x, y, level, right)
