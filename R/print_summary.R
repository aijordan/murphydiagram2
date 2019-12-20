#' @importFrom utils head tail
#'
#' @export
print.murphydiag <- function(x, ..., width = getOption("width")) {
  attribs <- attributes(x)
  x_order <- order_bymax(x)
  nm <- names(x)
  nm_ranked <- nm[x_order]
  
  nm_lens <- nchar(nm)
  prefix <- sprintf("murphydiag [1:%i] ", length(x))
  infix <- " "
  suffix <- " ..."
  too_long <- width - nchar(prefix) - nchar(suffix) <
    cumsum(nm_lens + nchar(infix)) - nchar(infix)
  namestring <- ifelse(
    any(too_long) && length(x) > 1L,
    paste0(paste(head(nm, max(1L, which.max(too_long) - 1L)), collapse = infix),
           suffix),
    paste(nm, collapse = infix)
  )
  
  nm_lens_ranked <- nm_lens[x_order]
  infix1 <- " < ... < "
  infix2 <- " < "
  cumlens <- cumsum(nm_lens_ranked + rev(nm_lens_ranked) + 2L * nchar(infix2)) -
    2L * nchar(infix2)
  too_long2 <- width - nchar(infix1) < cumlens[seq_len(floor(0.5 * length(x)))]
  n2 <- max(1L, which.max(too_long2) - 1L)
  rankstring <- ifelse(
    any(too_long2) && length(x) > 2L,
    paste(sep = infix1,
      paste(head(nm_ranked, n2), collapse = infix2),
      paste(tail(nm_ranked, n2), collapse = infix2)
    ),
    paste(nm_ranked, collapse = infix2)
  )
  
  models <- ifelse(
    identical(length(x), 0L),
    "murphydiag(0) to be combined",
    paste0(prefix, namestring)
  )
  predictions <- switch(
    attribs$functional$type,
    prob =,
    mean =,
    median = sprintf("with '%s' predictions", attribs$functional$type),
    expectile =,
    quantile = sprintf(
      "with '%s' predictions at level %.2f",
      attribs$functional$type,
      attribs$functional$level
    )
  )
  observations <- sprintf("for %i observations", nobs(x))
  prelim_ranking <- c(
    "",
    "Preliminary ranking by maximum value:",
    rankstring
  )
  
  cat(sep = "\n",
    models,
    paste(predictions, observations),
    if (length(x) > 1L) prelim_ranking
  )
  invisible(x)
}

order_bymax <- function(m) {
  max_values <- sapply(m, function(d) d$values$max)
  order(max_values)
}

#' Range of values in Murphy diagrams
#' 
#' Retrieve the range of knots and/or values
#' across all entries of one or multiple \code{'murphydiag'} objects.
#' Other objects are coerced by \code{as.murphydiag}.
#' 
#' @param ... one or multiple objects inheriting from the class \code{'murphydiag'}.
#' @param dim a character vector that specifies
#'  along which dimensions the range should be computed.
#' @param na.rm for compatibility with the S3 generic function.
#' 
#' @return
#'  a numeric vector of the form \code{c(k1, k2, v1, v2)},
#'  where
#'  \enumerate{
#'  \item the smallest knot \code{k1} and largest knot \code{k2}
#'  are dropped if \code{dim} does not contain the string \code{"knots"},
#'  \item the smallest value \code{v1} and largest value \code{v2}
#'  are dropped if \code{dim} does not contain the string \code{"values"}.
#'  }
#' 
#' @export
range.murphydiag <- function(..., dim = c("knots", "values"), na.rm = FALSE) {
  m <- c(...)
  xlim <- if ("knots" %in% dim) {
    if (identical(attr(m, "functional")$type, "prob")) {
      c(0, 1)
    } else {
      range(attr(m, "y"), sapply(m, function(d) range(d$x)))
    }
  } else NULL
  
  ylim <- if ("values" %in% dim) {
    c(0, max(sapply(m, function(d) d$values$max)))
  } else NULL
  
  c(xlim, ylim)
}
