#' dominance
#' 
#' @param m An object inheriting from the "murphydiag" class.
#' 
#' @return A matrix representing the dominance relationships (partial order)
#'   of the forecast methods in \code{m}.
#' 
#' @export
dominance <- function(m) {
  stopifnot(inherits(m, "murphydiag"))
  n <- length(m$x)
  partialOrder <- matrix(NA, n, n, dimnames = rep(list(names(m$x)), 2L))
  for (i in 1:n) {
    partialOrder[i, i] <- TRUE
    j <- i + 1
    while (j <= n) {
      tmp <- dominates(-m[c(i, j)])
      partialOrder[i, j] <- tmp[1L]
      partialOrder[j, i] <- tmp[2L]
      j <- j + 1
    }
  }
  class(partialOrder) <- "dominance"
  partialOrder
}

dominates <- function(m_diff) {
  stopifnot(inherits(m_diff, "murphydiag_diff"))
  vals <- m_diff$md_diff_fun("knots", "both")
  c(all(vals <= 0 | abs(vals) < sqrt(.Machine$double.eps)),
    all(vals >= 0 | abs(vals) < sqrt(.Machine$double.eps)))
}

