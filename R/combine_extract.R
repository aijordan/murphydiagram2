#' Combining Murphy diagram objects
#' 
#' Combine two or more \code{'murphydiag'} objects that are based on the same observations and type of forecasts. Other objects are coerced by \code{\link{as.murphydiag}} before combination.
#' 
#' 
#' @param ... objects to be concatenated.
#' @inheritParams as.murphydiag
#' 
#' @return an object inheriting from the class \code{'murphydiag'}.
#' 
#' @seealso \code{\link{as.murphydiag}}, \code{\link{[.murphydiag}}.
#' 
#' @export
c.murphydiag <- function(..., newdata = NULL, tol = sqrt(.Machine$double.eps)) {
  input <- list(...)
  attribs <- attributes_without_names(input[[1L]])
  
  m <- lapply(input, as.murphydiag, m = input[[1L]], newdata = newdata, tol = tol)
  m_lens <- lengths(m, use.names = FALSE)
  m <- unlist(m, recursive = FALSE)
  m_lens <- m_lens[m_lens > 0L]
  if (any(duplicated(names(m)))) {
    names(m) <- paste0("D", rep.int(seq_along(m_lens), m_lens), "_", names(m))
  }
  
  attributes(m) <- c(attributes(m), attribs)
  m
}


#' Subsetting Murphy diagram objects
#' 
#' @param x an object inheriting from the class \code{'murphydiag'}.
#' @param i index specifying which elements to extract.
#' 
#' @return an object inheriting from the class \code{'murphydiag'}.
#' 
#' @seealso \code{\link{c.murphydiag}}.
#' 
#' @export
`[.murphydiag` <- function(x, i) {
  attribs <- attributes_without_names(x)
  class(x) <- NULL
  x <- x[i]
  attributes(x) <- c(attributes(x), attribs)
  x
}


#' Subsetting dominance objects
#' 
#' @param d an object inheriting from the class \code{dominance}.
#' @param j index specifying elements to extract.
#' 
#' @return (see \link{[.data.frame}).
#' 
#' @export
`[.dominance` <- function(d, j) {
  class(d) <- NULL
  d <- d[j, j, drop = FALSE]
  class(d) <- "dominance"
  d
}
