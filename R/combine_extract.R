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
c.murphydiag <- function(...,
                         tol = sqrt(.Machine$double.eps),
                         newdata = NULL) {
    
  input <- list(...)
  proto <- input[[1L]]
  attribs <- attributes_without_names(proto)
  
  m <- purrr::map(
    .x = input,
    .f = as.murphydiag,
    m = proto,
    .name_repair = "minimal",
    tol = tol,
    newdata = newdata)
  m <- unlist(m, recursive = FALSE)
  names(m) <- vctrs::vec_as_names(names(m), repair = "unique")
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
