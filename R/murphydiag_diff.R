#' Murphy diagram difference object
#' 
#' \code{murphydiag_diff} constructors
#' 
#' @param m1 an object inheriting from class \code{murphydiag}.
#' @param m2 optional; an object that can be combined
#'   into an existing \code{murphydiag} object
#'   (see \link{c.murphydiag}).
#'   
#' @return returns a \code{murphydiag_diff} object to be used
#'   with \code{plot} (see \link{plot.murphydiag_diff})
#'   
#' @seealso \code{\link{murphydiag}},
#'   \code{\link{c.murphydiag}},
#'   \code{\link{[.murphydiag}},
#'   \code{\link{plot.murphydiag_diff}}
#'   
#' 
#' @export
murphydiag_diff <- function(m1, m2 = NULL) {
  stopifnot(inherits(m1, "murphydiag"))
  m1 - m2
}

#' @describeIn murphydiag_diff
#' 
#' @usage \method{-}{murphydiag}(m1, m2)
#' 
#' @export
`-.murphydiag` <- function(m1, m2 = NULL) {
  m <- c(m1, m2)
  if (!identical(length(m$x), 2L))
    stop("difference objects require exactly 2 murphy diagrams")
  m$md_diff_fun <- switch(
    m$functional$type,
    median =,
    quantile = pwConstantFun_diff(m),
    mean =,
    prob =,
    expectile = pwLinearFun_diff(m)
  )
  class(m) <- "murphydiag_diff"
  m
}