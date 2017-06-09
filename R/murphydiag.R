#' Murphy diagram object
#' 
#' \code{murphydiag} constructs and returns an object of
#' class \code{murphydiag}. It is a generic function with
#' a \code{default} method, and additional methods for classes
#' \code{lm}, \code{XXX}, \code{YYY}.
#' 
#' @param object an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @return The output is an object of class \code{murphydiag},
#'   which is a list containing the following components:
#'   \tabular{ll}{
#'     \code{x} \tab a data frame of forecasts\cr
#'     \code{y} \tab a vector of observations\cr
#'     \code{functional} \tab a list that specifies the functional,
#'       containing \code{type} and \code{level} (optional).
#'   }
#'
#' @export
murphydiag <- function(object, ...) UseMethod("murphydiag")

#' @describeIn murphydiag
#' 
#' @param y an object convertible by \code{\link{as.vector}}
#'   containing observations.
#' @param type a string specifying the type of forecast,
#'   e.g. \code{"mean"}.
#' @param level optional; single value in (0, 1);
#'   only required for certain values of \code{type}.
#' @param xnames optional; a character vector with the
#'   forecasting methods' names.
#' 
#' @seealso \code{\link{murphydiag_diff}},
#'   \code{\link{c.murphydiag}},
#'   \code{\link{[.murphydiag}},
#'   \code{\link{plot.murphydiag}}
#' 
#' @export
murphydiag.default <- function(object, y, type, level = NULL,
                               xnames = NULL, ...) {
  x <- object
  rval <- list(
    x = as.data.frame(x),
    y = as.vector(y, mode = "numeric"),
    functional = list(type = as.character(type)))
  
  if (!is.null(xnames)) names(rval$x) <- xnames
  
  stopifnot(all(sapply(rval$x, is.numeric)),
            identical(dim(rval$x)[1L], length(y)),
            identical(length(rval$functional$type), 1L))
  
  if (rval$functional$type %in% c("quantile", "expectile")) {
    rval$functional$level <- as.numeric(level)
    
    with(rval$functional,
         stopifnot(identical(length(level), 1L),
                   level > 0 && level < 1))
  }
  
  class(rval) <- "murphydiag"
  rval
}

#' @describeIn murphydiag
#' 
#' @param newdata optional; a data frame as in
#'   \code{\link{predict.lm}} leading to forecasts
#'   for \code{newy}.
#' @param newy optional; a vector of observations
#'   corresponding to forecasts based on \code{newdata}.
#'
#' @importFrom stats predict
#' @export
murphydiag.lm <- function(object,
                          newdata = NULL,
                          newy = NULL,
                          type = "mean",
                          xnames = NULL, ...) {
  if (is.null(xnames)) xnames <- "lm"
  if (is.null(newy) || is.null(newdata)) {
    if (!is.null(newy)) warning("ignored 'newy' since 'newdata' is NULL")
    if (!is.null(newdata)) warning("ignored 'newdata' since 'newy' is NULL")
    newy <- object$fitted.values + object$residuals
    newdata <- NULL
  }
  if (!type %in% c("mean", "median")) {
    stop("only forecast types 'mean' and 'median' are implemented")
  }
  
  murphydiag(predict(object, newdata), newy, type, xnames = xnames)
}