#' Converting a murphydiag object into a data.frame
#' 
#' @inheritParams plot.murphydiag
#' 
#' @return A \code{data.frame} with three columns: \code{method}, \code{threshold}, \code{score}
#'
#' @export
as.data.frame.murphydiag <- function(x, ..., thresholds = 1e4L, right.cont = "both") {
  m <- x
  if (identical(m$functional$type, "prob")) {
    range_tt <- c(0, 1)
    xlim <- range_tt
  } else {
    range_tt <- range(m$x, m$y)
    xlim <- range_tt + c(-.05, .05) * (range_tt[2L] - range_tt[1L])
  }
  
  knots <- knots(m)
  msfun <- ms_fun(m)
  if (length(thresholds) == 1L && any(lengths(knots) > thresholds)) {
    stopifnot(thresholds > 2L)
    xx <- seq(xlim[1L], xlim[2L], length.out = thresholds)
    yy <- msfun(xx, right.cont)
    if (right.cont == "both") xx <- rep(xx, each = 2L)
    method <- rep(names(m$x), each = length(xx))
    rval <- data.frame(
      method = method,
      threshold = xx,
      score = c(yy),
      stringsAsFactors = FALSE
    )
  } else if (length(thresholds) > 1L) {
    xx <- sort(unique(thresholds))
    yy <- msfun(xx, right.cont)
    if (right.cont == "both") xx <- rep(xx, each = 2L)
    method <- rep(names(m$x), each = length(xx))
    rval <- data.frame(
      method = method,
      threshold = xx,
      score = c(yy),
      stringsAsFactors = FALSE
    )
  } else {
    yy0 <- msfun(xlim[1L], right.cont, FALSE)
    yy1 <- msfun("knots", right.cont, FALSE)
    yy2 <- msfun(xlim[2L], right.cont, FALSE)
    yy <- mapply(c, yy0, yy1, yy2, SIMPLIFY = FALSE)
    xx <- mapply(c, xlim[1L], knots, xlim[2L], SIMPLIFY = FALSE)
    if (right.cont == "both") xx <- mapply(rep, xx, each = 2L, SIMPLIFY = FALSE)
    rval <- do.call(
      rbind,
      lapply(
        seq_along(knots),
        function(i) {
          data.frame(
            method = names(m$x)[i],
            threshold = xx[[i]],
            score = yy[[i]],
            stringsAsFactors = FALSE
          )
        }
      )
    )
  }
  
  unique(rval)
}


#' @export
as.murphydiag <- function(object, m, ...) UseMethod("as.murphydiag")

#' @export
as.murphydiag.default <- function(object, m, xnames = NULL, ...) {
  x <- as.data.frame(object)
  if (!is.null(xnames)) names(x) <- xnames
  
  as.murphydiag(x, m)
}

#' @export
as.murphydiag.data.frame <- function(object, m, xnames = NULL, ...) {
  stopifnot(inherits(m, "murphydiag"))
  y <- attributes(m)$y
  
  # checkObject()
  x <- object
  stopifnot(
    all(sapply(object, is.numeric)),
    identical(nrow(x), 0L) || identical(nrow(x), length(y)),
    !anyNA(x))
  if (attributes(m)$functional$type == "prob") {
    stopifnot(all(x >= 0 | x <= 1))
  }
  
  ordery <- order(y)
  x <- switch(
    attributes(m)$functional$type,
    prob = ,
    mean = lapply(x, C_md_expect, y, 0.5, ordery),
    expectile = lapply(x, C_md_expect, y, attributes(m)$functional$level, ordery),
    median = lapply(x, C_md_quant, y, 0.5, ordery),
    quantile = lapply(x, C_md_quant, y, attributes(m)$functional$level, ordery)
  )
  attributes(x) <- c(attributes(x), attributes_without_names(m))
  x
}

#' @export
as.murphydiag.murphydiag <- function(object, m, tol = sqrt(.Machine$double.eps), ...) {
  stopifnot(inherits(m, "murphydiag"))
  ref <- attributes_without_names(m)
  convertFunctional <- switch(
    ref$functional$type,
    median =,
    prob =,
    mean = simplifyFunctional,
    quantile =,
    expectile = generalizeFunctional
  )
  x <- convertFunctional(object)
  stopifnot(
    isTRUE(all.equal(attributes(x)$y, ref$y, tolerance = tol)),
    identical(attributes(x)$functional, ref$functional)
  )
  x
}

#' @export
simplifyFunctional <- function(m, tol = sqrt(.Machine$double.eps)) {
  if (isTRUE(abs(attr(m, "functional")$level - 0.5) < tol)) {
    attr(m, "functional")$type <- switch(
      attr(m, "functional")$type, quantile = "median", expectile = "mean"
    )
    attr(m, "functional")$level <- NULL
  }
  m
}

#' @export
generalizeFunctional <- function(m) {
  type <- attr(m, "functional")$type
  if (isTRUE(type %in% c("median", "prob", "mean"))) {
    attr(m, "functional")$type <- switch(
      type, median = "quantile", prob = , mean = "expectile"
    )
    attr(m, "functional")$level <- 0.5
  }
  m
}
