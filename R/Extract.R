#' Subsetting Murphy diagram objects
#' 
#' @param m an object inheriting from the class \code{murphydiag}.
#' @param j index specifying elements to extract.
#' 
#' @return (see \link{[.data.frame}).
#' 
#' @seealso \code{\link{c.murphydiag}}
#' 
#' @export
`[.murphydiag` <- function(m, j) {
  m$x <- m$x[, j, drop = FALSE]
  m$md_fun <- m$md_fun[j]
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
