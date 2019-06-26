#' Combining Murphy diagram objects
#' 
#' Combine two or more 'murphydiag' objects that are based on the same observations and type of forecasts.
#' 
#' @param ... objects to be concatenated.
#' 
#' @return An object inheriting from the \code{murphydiag} class.
#' 
#' @seealso \code{\link{[.murphydiag}}, \code{\link{murphydiag}}
#' 
#' @export
c.murphydiag <- function(..., xnames = NULL, tol = sqrt(.Machine$double.eps)) {
  input <- list(...)
  attribs <- attributes_without_names(input[[1L]])
  m <- lapply(input, as.murphydiag, m = input[[1L]], tol = tol)
  m <- unlist(m, recursive = FALSE)
  attributes(m) <- c(attributes(m), attribs)
  m
}


#' Subsetting Murphy diagram objects
#' 
#' @param m an object inheriting from the class \code{murphydiag}.
#' @param i index specifying elements to extract.
#' 
#' @return (see \link{[.data.frame}).
#' 
#' @seealso \code{\link{c.murphydiag}}
#' 
#' @export
`[.murphydiag` <- function(m, i) {
  attribs <- attributes_without_names(m)
  class(m) <- NULL
  m <- m[i]
  attributes(m) <- c(attributes(m), attribs)
  m
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
