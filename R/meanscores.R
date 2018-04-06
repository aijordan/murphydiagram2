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
