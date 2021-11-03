#' @useDynLib murphydiagram2
#' @importFrom Rcpp sourceCpp
NULL

#' Murphy diagram object
#' 
#' Documentation of the \code{'murphydiag'} object, and its constructors.
#' 
#' \code{murphydiag} constructs and returns an object inheriting from the
#' class \code{'murphydiag'}. It is a wrapper function around the
#' concatenation function described in \code{\link{c.murphydiag}}, which
#' in turn calls the underlying
#' coercion methods described in \code{\link{as.murphydiag}}.
#' 
#' \code{murphydiag0} constructs an empty \code{'murphydiag'} object from
#' response values and the type of predictions that are expected.
#' 
#' Numeric predictions are ideally come with additional meta information
#' about their type. Objects of the class \code{'murphydiag'} can be constructed
#' from probability predictions for binary outcomes (\code{type = "prob"}),
#' from mean predictions for real-valued outcomes (\code{type = "mean"}),
#' or from median predictions (\code{type = "median"}). As a
#' generalization of median predictions, quantile predictions at level \eqn{\alpha}
#' (e.g., \code{type = "quantile"} and \code{level = 0.5}) are also implemented.
#' In the same way as a quantile is a generalization of the median, we can
#' generalize mean predictions by expectile predictions at level \eqn{\alpha}
#' (e.g., \code{type = "expectile"} and \code{level = 0.5}).
#' 
#' When evaluating the quality of predictions, the prediction meta information
#' influences the set of admissible scoring functions. The elementary scoring
#' functions used in this package are as follows:
#' \itemize{
#' \item probability predictions:
#'   \eqn{s(\theta; x, y) = (1(\theta < x) - 1(\theta < y)) 2(\theta - y)}
#' \item mean predictions:
#'   \eqn{s(\theta; x, y) = (1(\theta < x) - 1(\theta < y)) 2(\theta - y)}
#' \item expectile predictions at level \eqn{\alpha}:
#'   \eqn{s(\theta, \alpha; x, y) = (1(\theta < x) - 1(\theta < y)) 4|1(y < \theta) - \alpha|(\theta - y)}
#' }
#' 
#' @param ... objects to be coerced to \code{'murphydiag'} and concatenated
#' @inheritParams as.murphydiag
#' 
#' @return
#'  \code{murphydiag} returns a \code{'murphydiag'} object,
#'  which is a named list-type vector class with attributes
#'  \tabular{ll}{
#'    \code{y} \tab a numeric vector of response values to be predicted.\cr
#'    \code{functional} \tab a list containing the specified \code{type}
#'    of forecast, and \code{level} if applicable.
#'  }
#'  Each entry of a \code{'murphydiag'} object is a list
#'  with the following components:
#'  \tabular{ll}{
#'    \code{x} \tab a numeric vector of predictions for \code{y}.\cr
#'    \code{knots} \tab a numeric vector of knots
#'    of the piecewise linear Murphy diagram.\cr
#'    \code{values} \tab a list containing numeric vectors
#'    of the left and right sided limits of the Murphy diagram values
#'    taken in the knots (\code{left} and \code{right}),
#'    and the maximal value (\code{max}).\cr
#'  }
#' 
#'  \code{murphydiag0} returns an empty \code{'murphydiag'} object
#'  with attributes \code{y} and \code{functional}.
#' 
#' @seealso
#'  \code{\link{c.murphydiag}},
#'  \code{\link{[.murphydiag}},
#'  \code{\link{plot.murphydiag}}.
#' 
#' @name murphydiag
NULL

#' @rdname murphydiag
#' 
#' @export
murphydiag <- function(...,
                       y = NULL, 
                       m = NULL,
                       type = NULL,
                       level = NULL,
                       newdata = NULL) {
    
  if ((!is.null(y) || !is.null(type)) && !is.null(m)) {
    stop("specify 'y' and 'type', or 'm', but not both")
  }
  if (is.null(m)) m <- murphydiag0(y, type, level)
  stopifnot(is.murphydiag(m))
  
  do.call(
    what = c,
    args = lapply(
      X = list(...),
      FUN = as.murphydiag,
      m = m,
      .name_repair = "minimal",
      newdata = newdata
    )
  )
}

#' @rdname murphydiag
#' 
#' @export
murphydiag0 <- function(y, type, level = NULL) {
  if (identical(length(level), 0L)) level <- NULL
  stopifnot(is.numeric(y))
  stopifnot(!anyNA(y))
  stopifnot(is.character(type))
  stopifnot(identical(length(type), 1L))
  stopifnot(is.null(level) || is.numeric(level))
  switch(
    type,
    prob = stopifnot(
      all(y == 0 | y == 1),
      is.null(level)),
    mean =,
    median = stopifnot(is.null(level)),
    quantile =,
    expectile = stopifnot(
      identical(length(level), 1L),
      level > 0 && level < 1),
    stop(sprintf("unknown functional type: '%s'", type))
  )
  
  m <- structure(list(), names = character(0))
  attr(m, "y") <- y
  attr(m, "y_order") <- order(y)
  attr(m, "functional")$type <- type
  attr(m, "functional")$level <- level
  class(m) <- "murphydiag"
  m
}
