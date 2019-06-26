#' dominance
#' 
#' @param m An object inheriting from the "murphydiag" class.
#' @param abs.tol absolute tolerance
#' 
#' @return A matrix representing the dominance relationships (partial order)
#'   of the forecast methods in \code{m}.
#' 
#' @export
dominance <- function(m, abs.tol = sqrt(.Machine$double.eps)) {
  stopifnot(inherits(m, "murphydiag"))
  n <- length(m)
  if (identical(n, 0L) || identical(n, 1L)) {
    partialOrder <- matrix(TRUE, n, n)
    dimnames(partialOrder) <- rep(list(names(m)), 2L)
    class(partialOrder) <- "dominance"
    return(partialOrder)
  }
  
  dominates <- switch(
    attributes(m)$functional$type,
    prob =,
    mean =,
    expectile = C_dominates_expect,
    median =,
    quantile = C_dominates_quant
  )
  
  partialOrder <- 
    sapply(1:n, function(j) {
      sapply(1:n, function(i) {
        if (i == j) return(TRUE)
        dominates(m[[i]], m[[j]], abs.tol)
      })
    })
  dimnames(partialOrder) <- rep(list(names(m)), 2L)
  class(partialOrder) <- "dominance"
  partialOrder
}

# diag(partialOrder) <- TRUE
# for (i in seq_len(n - 1)) {
#   for (j in (i + 1):n) {
#     if (is.na(partialOrder[i, j])) {
#       partialOrder[i, j] <- dominates(m[[i]], m[[j]], abs.tol)
#     }
#     if (is.na(partialOrder[j, i])) {
#       partialOrder[j, i] <- dominates(m[[j]], m[[i]], abs.tol)
#     }
#   }
#   remaining <- (i + 1):n
#   which_i_dom <- split(remaining, partialOrder[i, remaining])
#   which_dom_i <- split(remaining, partialOrder[remaining, i])
#   if (!is.null(which_dom_i$`TRUE`) && !is.null(which_i_dom$`TRUE`)) {
#     partialOrder[which_dom_i$`TRUE`, which_i_dom$`TRUE`] <- TRUE
#   }
#   if (identical(length(which_dom_i), 2L)) {
#     partialOrder[which_dom_i$`FALSE`, which_dom_i$`TRUE`] <- FALSE
#   }
#   if (identical(length(which_i_dom), 2L)) {
#     partialOrder[which_i_dom$`TRUE`, which_i_dom$`FALSE`] <- FALSE
#   } 
# }