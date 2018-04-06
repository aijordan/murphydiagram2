#' Murphy diagram object
#' 
#' \code{murphydiag} constructs and returns an object of
#' class \code{murphydiag}. It is a generic function with
#' a \code{default} method, and additional methods for classes
#' \code{\link[stats]{lm}}, \code{\link[randomForest]{randomForest}}, \code{\link[quantreg]{rq}} (from package \code{quantreg}).
#' 
#' @param object an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @return The output is an object of class \code{murphydiag},
#'   which is a list containing the following components:
#'   \tabular{ll}{
#'     \code{x} \tab a data frame of forecasts\cr
#'     \code{y} \tab a vector of observations\cr
#'     \code{functional} \tab a list that specifies the forecast functional,
#'       containing \code{type} and \code{level} (optional).
#'   }
#'
#' @details In the default version, the user specifies all relevant information (forecasts, realizations, 
#' information on the type of forecast) manually. Furthermore, \code{murphydiag} accepts fitted model objects
#' from a few other packages:
#' \itemize{
#' \item \dQuote{lm}, see \code{\link[stats]{lm}}
#' \item \dQuote{rq}, see \code{\link[quantreg]{rq}}
#' \item \dQuote{randomForest}, see \code{\link[randomForest]{randomForest}}
#' }
#' 
#' @seealso \code{\link{murphydiag_diff}},
#'   \code{\link{c.murphydiag}},
#'   \code{\link{[.murphydiag}},
#'   \code{\link{plot.murphydiag}}
#' 
#' @export
murphydiag <- function(object, ...) UseMethod("murphydiag")

#' @rdname murphydiag
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
#' @export
murphydiag.default <- function(object, y, type, level = NULL, xnames = NULL, ...) {
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

#' @rdname murphydiag
#' 
#' @param newdata optional; a data frame as in
#'   \code{\link{predict.lm}} leading to forecasts
#'   for \code{newy}.
#' @param newy optional; a vector of observations
#'   corresponding to forecasts based on \code{newdata}.
#'
#' @importFrom stats predict
#' 
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

#' @rdname murphydiag
#' 
#' @export
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

#' @rdname murphydiag
#' 
#' @export
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
