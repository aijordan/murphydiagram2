#' Coerce a Murphy diagram to a Data Frame
#' 
#' @param x \code{'murphydiag'} object to be coerced to \code{'data.frame'}
#' @inheritParams base::as.data.frame
#' 
#' @return
#'  Returns a \code{'data.frame'} with four columns:
#'  \code{name}, \code{knot}, \code{limit}, \code{value}
#'  
#' @details
#'  Coercion of a \code{'murphydiag'} object to a \code{'data.frame'}
#'  retains only the relevant information for plotting: the model/forecast
#'  name, the knots (break points) of the piecewise linear functions,
#'  and the values taken in the left and right limits in the knots.
#'  
#'  Additional information such as the observations, the predictions,
#'  and the functional type of the predictions is lost. Hence, reversion
#'  to \code{'murphydiag'} is impossible. When coercing a
#'  \code{'data.frame'} to a \code{'murphydiag'}, it is assumed that the
#'  \code{'data.frame'} comprises predictions (from multiple methods).
#'
#' 
#' @export
as.data.frame.murphydiag <- function(x, ...) {
  if (identical(length(x), 0L)) return(data.frame())
  if (identical(length(attr(x, "y")), 0L)) return(data.frame())
  
  df_list <- lapply(seq_along(x), function(i) {
    d <- x[[i]]
    data.frame(stringsAsFactors = FALSE,
      name = names(x)[i],
      knot = rep(d$knots, each = 2L),
      limit = c("left", "right"),
      value = `dim<-`(rbind(d$values$left, d$values$right), NULL)
    )
  })
  do.call(rbind, df_list)
}

#' Coerce to a Murphy diagram
#' 
#' generic function with
#' a \code{default} method, and additional methods for classes
#' \code{\link[stats]{lm}}, \code{\link[randomForest]{randomForest}}, \code{\link[quantreg]{rq}} (from package \code{quantreg}).
#' 
#' In the default version, the user specifies all relevant information (forecasts, realizations, 
#' information on the type of forecast) manually. Furthermore, \code{murphydiag} accepts fitted model objects
#' from a few other packages:
#' \itemize{
#' \item \dQuote{lm}, see \code{\link[stats]{lm}}
#' \item \dQuote{rq}, see \code{\link[quantreg]{rq}}
#' \item \dQuote{randomForest}, see \code{\link[randomForest]{randomForest}}
#' }
#' 
#' @param x any \R object.
#' @param y a numeric vector of response values to be predicted.
#' @param type a string specifying the type of forecast; one of \code{"prob"}, \code{"mean"}, \code{"expectile"}, \code{"median"}, \code{"quantile"}.
#' @param level a single numeric value for the level of the \code{"quantile"} or \code{"expectile"} functional.
#' @param m an object inheriting from the class \code{'murphydiag'}; alternative to \code{y}, \code{type}, \code{level}.
#' @param xnames a character vector of prediction names.
#' @param tol accuracy when comparing \code{y} and \code{level} in \code{'murphydiag'} objects.
#' @param newdata an optional data frame used as environment in which to evaluate \code{predict} methods of model objects.
#' 
#' @details
#'  as.murphydiag.randomForest converts the classification problem to a binary problem with the first class treated as 1 and all other classes as 0
#'  
#' @return
#'  \code{as.murphydiag} returns a \code{'murphydiag'} object.
#'  
#'  \code{is.murphydiag} returns \code{TRUE} if its argument is a
#'  Murphy diagram, that is, has \code{"murphydiag"} among its classes,
#'  and \code{FALSE} otherwise.
#'  
#'  \code{simplifyFunctional} coerces \code{type} and \code{level} of a
#'  \code{'murphydiag'} object from \code{"quantile"} to \code{"median"},
#'  or \code{"expectile"} to \code{"mean"}, if possible. That is,
#'  when \code{level} is close enough to 0.5.
#'  
#'  \code{generalizeFunctional} coerces \code{type} of a \code{'murphydiag'}
#'  object from \code{"prob"} or \code{"mean"} to \code{"expectile"} with
#'  \code{level} at 0.5, or \code{"median"} to \code{"quantile"} with
#'  \code{level} at 0.5.
#'  
#' @name as.murphydiag
NULL

#' @rdname as.murphydiag
#' 
#' @export
as.murphydiag <- function(x, ...) {
  UseMethod("as.murphydiag")
}

#' @rdname as.murphydiag
#' 
#' @export
is.murphydiag <- function(x) {
  inherits(x, "murphydiag")
}

#' @rdname as.murphydiag
#' 
#' @export
as.murphydiag.default <- function(x,
                                  y = NULL,
                                  type = NULL,
                                  level = NULL,
                                  m = NULL,
                                  .name_repair = "unique",
                                  ...) {
    
  if ((!is.null(y) || !is.null(type) || !is.null(level)) && !is.null(m)) {
    stop("specify 'y', 'type', and 'level', or 'm', but not both")
  }
  if (is.null(m)) m <- murphydiag0(y, type, level)
  stopifnot(is.murphydiag(m))
  
  x <- as.data.frame(x, optional = TRUE, fix.empty.names = FALSE) |>
    tibble::as_tibble(.name_repair = .name_repair)
  
  as.murphydiag.data.frame(
    x,
    m = m,
    .name_repair = .name_repair,
    ...
  )
}

#' @rdname as.murphydiag
#' 
#' @export
as.murphydiag.data.frame <- function(x,
                                     y = NULL,
                                     type = NULL,
                                     level = NULL, 
                                     m = NULL,
                                     .name_repair = "unique",
                                     ...) {
  
  if ((!is.null(y) || !is.null(type) || !is.null(level)) && !is.null(m)) {
    stop("specify 'y', 'type', and 'level', or 'm', but not both")
  }
  if (is.null(m)) m <- murphydiag0(y, type, level)
  stopifnot(is.murphydiag(m))
  if (is.null(y)) y <- attr(m, "y")
  attribs <- attributes_without_names(m)
  
  stopifnot(all(sapply(x, is.numeric)))
  stopifnot(identical(nrow(x), 0L) || identical(nrow(x), length(y)))
  
  x <- tibble::as_tibble(x, .name_repair = .name_repair)
  
  ordery <- order(y)
  m <- lapply(
    X = x,
    FUN = murphydiag_numeric,
    m = m
  )
  
  attributes(m) <- c(attributes(m), attribs)
  m
}
  
# returns an unnamed murphydiag
murphydiag_numeric <- function(x, m = NULL, ...) {
  
  stopifnot(is.murphydiag(m))
  y <- attr(m, "y")
  ordery <- attr(m, "y_order")
  ftype <- attr(m, "functional")$type
  flevel <- attr(m, "functional")$level
  
  stopifnot(identical(length(x), length(y)))
  if (ftype == "prob") {
    stopifnot(all(x >= 0 | x <= 1))
  }
  stopifnot(!anyNA(x))
  
  switch(
    ftype,
    prob = ,
    mean =      C_md_expect(x, y,    0.5, ordery),
    expectile = C_md_expect(x, y, flevel, ordery),
    median =    C_md_quant( x, y,    0.5, ordery),
    quantile =  C_md_quant( x, y, flevel, ordery)
  )
  
}
  

#' @rdname as.murphydiag
#' 
#' @export
as.murphydiag.murphydiag <- function(x,
                                     y = NULL,
                                     type = NULL,
                                     level = NULL,
                                     m = NULL,
                                     tol = sqrt(.Machine$double.eps),
                                     ...) {
    
  if ((!is.null(y) || !is.null(type) || !is.null(level)) && !is.null(m)) {
    stop("specify 'y', 'type', and 'level', or 'm', but not both")
  }
  if (is.null(m)) m <- murphydiag0(y, type, level)
  stopifnot(is.murphydiag(m))
  
  ref <- attributes_without_names(m)
  convertFunctional <- switch(
    ref$functional$type,
    median =,
    prob =,
    mean = simplifyFunctional,
    quantile =,
    expectile = generalizeFunctional
  )
  x <- convertFunctional(x)
  stopifnot(
    isTRUE(all.equal(attributes(x)$y, ref$y, tolerance = tol)),
    identical(attributes(x)$functional, ref$functional)
  )
  x
}

#' @rdname as.murphydiag
#'
#' @importFrom stats predict
#' 
#'
as.murphydiag.lm <- function(x, y = NULL, type = NULL, level = NULL, m = NULL,
                             xnames = NULL, newdata = NULL, ...) {
  if ((!missing(y) || !missing(type) || !missing(level)) && !missing(m)) {
    stop("specify 'y', 'type', and 'level', or 'm', but not both")
  }
  if (is.null(m)) {
    if (is.null(y)) y <- x$fitted.values + x$residuals
    if (is.null(type)) type <- "mean"
    m <- murphydiag0(y, type, level)
  }
  stopifnot(is.murphydiag(m))
  if (!attr(simplifyFunctional(m), "functional")$type %in% c("mean", "median")) {
    warning("For objects of class 'lm': ",
            "Recommended forecast types are 'mean' or 'median'.")
  }
  
  predictions <- list(predict(x, newdata))
  if (is.null(xnames)) xnames <- "lm"
  names(predictions) <- xnames
  c(m[NULL], predictions)
}

#' @rdname as.murphydiag
#'
#' @importFrom stats predict
#' 
#'
as.murphydiag.rq <- function(x, y = NULL, type = NULL, level = NULL,
                             m = NULL, xnames = NULL, newdata = NULL, ...) {
  if (!requireNamespace("quantreg")) {
    stop("quantreg package could not be loaded")
  }
  if ((!missing(y) || !missing(type) || !missing(level)) && !missing(m)) {
    stop("specify 'y', 'type', and 'level', or 'm', but not both")
  }
  if (is.null(m)) {
    if (is.null(y)) y <- x$y
    if (is.null(type)) type <- "quantile"
    if (is.null(level)) level <- x$tau
    m <- murphydiag0(y, type, level)
  }
  stopifnot(is.murphydiag(m))
  if (!attr(m, "functional")$type %in% c("median", "quantile")) {
    warning("For objects of class 'rq': ",
            "Recommended forecast types are 'quantile' or 'median'.")
  }
  
  predictions <- if (is.null(newdata)) {
    list(predict(x))
  } else {
    list(predict(x, newdata))
  }
  if (is.null(xnames)) xnames <- "rq"
  names(predictions) <- xnames
  c(m[NULL], predictions)
}

#' @rdname as.murphydiag
#'
#' @importFrom stats predict
#' 
#'
as.murphydiag.randomForest <- function(x, y = NULL, type = NULL, level = NULL,
                                       m = NULL, xnames = NULL, newdata = NULL,
                                       ...) {
  if (!requireNamespace("randomForest")) {
    stop("randomForest package could not be loaded")
  }
  if ((!missing(y) || !missing(type) || !missing(level)) && !missing(m)) {
    stop("specify 'y', 'type', and 'level', or 'm', but not both")
  }
  if (is.null(m)) {
    if (is.null(y)) y <- x$y
    if (is.null(type)) {
      if (x$type == "classification") {
        type <- "prob"
        if (is.factor(y)) y <- as.numeric(y == levels(y)[1L])
      } else if (x$type == "regression") {
        type <- "mean"
      }
    }
    m <- murphydiag0(y, type, level)
  }
  stopifnot(is.murphydiag(m))
  if (!attr(m, "functional")$type %in% c("prob", "mean")) {
    warning("For objects of class 'rq': ",
            "Recommended forecast types are 'prob' or 'mean'.")
  }
  
  predictions <- if (x$type == "classification") {
    if (is.null(newdata)) {
      predict(x, type = "prob")[, 1L]
    } else {
      predict(x, newdata, type = "prob")[, 1L]
    }
  } else if (x$type == "regression") {
    if (is.null(newdata)) {
      predict(x)
    } else {
      predict(x, newdata)
    }
  }
  if (is.null(xnames)) xnames <- "randomForest"
  names(predictions) <- xnames
  c(m[NULL], predictions)
}

#' @rdname as.murphydiag
#' 
#' @export
simplifyFunctional <- function(m, tol = sqrt(.Machine$double.eps)) {
  stopifnot(is.murphydiag(m))
  if (isTRUE(abs(attr(m, "functional")$level - 0.5) < tol)) {
    attr(m, "functional")$type <- switch(
      attr(m, "functional")$type, quantile = "median", expectile = "mean"
    )
    attr(m, "functional")$level <- NULL
  }
  m
}

#' @rdname as.murphydiag
#' 
#' @export
generalizeFunctional <- function(m) {
  stopifnot(is.murphydiag(m))
  type <- attr(m, "functional")$type
  if (isTRUE(type %in% c("median", "prob", "mean"))) {
    attr(m, "functional")$type <- switch(
      type, median = "quantile", prob = , mean = "expectile"
    )
    attr(m, "functional")$level <- 0.5
  }
  m
}
