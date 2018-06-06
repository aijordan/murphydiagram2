#' Getting the 'knots' from a "murphydiag" object
#' 
#' @param Fn an R object inheriting from "murphydiag"
#' @param ... aditional parameters.
#' 
#' @return A list of knot vectors; one for each forecasting method in \code{Fn}.
#' 
#' @importFrom stats knots
#' 
#' @export
knots.murphydiag <- function(Fn, ...) {
  lapply(Fn$md_fun, function(FUN) eval(expression(knots), environment(FUN)))
}

intercepts <- function(Fn, ...) {
  lapply(Fn$md_fun, function(FUN) eval(expression(a), environment(FUN)))
}

slopes <- function(Fn, ...) {
  lapply(Fn$md_fun, function(FUN) {
    eval(expression(tryCatch(b, error = function(e) NULL)), environment(FUN))
  })
}
