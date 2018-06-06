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
c.murphydiag <- function(...) {
  input <- list(...)
  input <- input[!sapply(input, is.null)]
  
  proto <- input[[1L]]
  inputm <- input[sapply(input, inherits, "murphydiag")]
  identical_y <-
    sapply(inputm,
           function(m1, m2) {
             isTRUE(all.equal(m1$y, m2$y))
           },
           proto)
  identical_func <-
    sapply(inputm,
           function(m1, m2) {
             identical(m1$functional, m2$functional)
           },
           proto)
  if (!all(identical_y))
    stop("incompatible observations")
  if (!all(identical_func))
    stop("incompatible functionals")
  
  inputx <-
    lapply(input,
           function(m) {
             if (inherits(m, "murphydiag"))
               m$x
             else
               m
           })
  
  rval <- list(
    x = as.data.frame(inputx),
    y = proto$y,
    functional = proto$functional)
  
  stopifnot(all(sapply(rval$x, is.numeric)),
            identical(dim(rval$x)[1L], length(rval$y)))
  
  f <- switch(
    proto$functional$type,
    mean = function(m) md_mean(m, proto$y),
    prob = function(m) md_prob(m, proto$y),
    median = function(m) md_median(m, proto$y),
    quantile = function(m) md_quant(m, proto$y, proto$functional$level),
    expectile = function(m) md_expect(m, proto$y, proto$functional$level)
  )
  
  inputmd_fun <- lapply(
    input,
    function(m) {
      if (inherits(m, "murphydiag")) {
        m$md_fun
      } else {
        f(m)
      }
    }
  )
  
  rval$md_fun <- do.call(c, inputmd_fun)
  
  class(rval) <- "murphydiag"
  rval
}
