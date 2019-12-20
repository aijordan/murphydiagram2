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
murphydiag <- function(..., y = NULL, type = NULL, level = NULL, m = NULL, newdata = NULL) {
  if ((!missing(y) || !missing(type)) && !missing(m)) {
    stop("specify 'y' and 'type', or 'm', but not both")
  }
  if (is.null(m)) m <- murphydiag0(y, type, level)
  stopifnot(is.murphydiag(m))
  
  c(m[NULL], ..., newdata = newdata)
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
  stopifnot(is.null(level) | is.numeric(level))
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
  attr(m, "functional")$type <- type
  attr(m, "functional")$level <- level
  class(m) <- "murphydiag"
  m
}

murphydiag.rq <- function(object,
                          newdata = NULL,
                          newy = NULL,
                          xnames = NULL, ...) {
  qr_pckg <- requireNamespace("quantreg")
  if (!qr_pckg) stop("quantreg package could not be loaded")
  if (is.null(xnames)) xnames <- "rq"
  if (is.null(newy) || is.null(newdata)) {
    if (!is.null(newy)) warning("ignored 'newy' since 'newdata' is NULL")
    if (!is.null(newdata)) warning("ignored 'newdata' since 'newy' is NULL")
    newy <- object$y
    newdata <- NULL
  }
  # Use type "median" for 50pct quantile (matters only for scale factor)
  type <- ifelse(object$tau == 0.5, "median", "quantile")
  level <- NULL
  if (type == "quantile") level <- object$tau
  # Output (case distinction since rq does not accept newdata = NULL)
  if (is.null(newdata)){
    murphydiag(predict(object), newy, type, level = level, xnames = xnames)
  } else {
    murphydiag(predict(object, newdata), newy, type, level = level, xnames = xnames)
  }
}

murphydiag.randomForest <- function(object,
                                    newdata = NULL,
                                    newy = NULL,
                                    xnames = NULL, ...) {
  rF_pckg <- requireNamespace("randomForest")
  if (!rF_pckg) stop("randomForest package could not be loaded")
  if (is.null(xnames)) xnames <- "randomForest"
  if (is.null(newy) || is.null(newdata)) {
    if (!is.null(newy)) warning("ignored 'newy' since 'newdata' is NULL")
    if (!is.null(newdata)) warning("ignored 'newdata' since 'newy' is NULL")
    newy <- object$y
    newdata <- NULL
  } else {
    # Check for consistency between training/test data
    if (class(object$y) != class(newy)) stop("'newy' does not match data used for model fitting")
  }
  # Stop in case of classification with more than two classes
  if (object$type == "classification" & length(unique(object$y)) > 2){
    stop("Murphy diagrams not applicable for qualitative data with more than two classes")
  }
  # Determine type
  if (object$type == "classification"){
    type <- "prob"
  } else if (object$type == "regression"){
    type <- "mean"
  }
  level <- NULL

  # Prediction
  if (type == "prob"){
    # Extract predicted probability for first class
    if (is.null(newdata)){
      pred <- predict(object, type = "prob")[,1]  
    } else {
      pred <- predict(object, newdata = newdata, type = "prob")[,1]  
    }
    # Change newy to binary variable if necessary
    if (is.factor(newy)){
      if (length(levels(newy)) != 2) stop("Can only handle two classes")
      newy <- newy == levels(newy)[1]
    }
  } else {
    # Extract mean prediction
    if (is.null(newdata)){
      pred <- predict(object)
    } else {
      pred <- predict(object, newdata = newdata)
    }
  }
  
  murphydiag(pred, newy, type, level = level, xnames = xnames)
  
}
