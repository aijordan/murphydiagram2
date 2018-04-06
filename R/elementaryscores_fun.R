#' Elementary score function constructors
#' 
#' Creating functions for the computation of mean elementary scores.
#' 
#' @param m an object inheriting from class \code{murphydiag}.
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
#' @export es_fun
es_fun <- function(m) {
  stopifnot(inherits(m, "murphydiag"))
  switch(m$functional$type,
         mean = es_mean_fun(m$x, m$y),
         prob = es_prob_fun(m$x, m$y),
         median = es_median_fun(m$x, m$y),
         quantile = es_quant_fun(m$x, m$y, m$functional$level),
         expectile = es_expect_fun(m$x, m$y, m$functional$level))
}

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


