#' Converting a murphydiag object into a data.frame
#' 
#' @inheritParams plot.murphydiag
#' 
#' @return A \code{data.frame} with three columns: \code{method}, \code{threshold}, \code{score}
#'
#' @export
as.data.frame.murphydiag <- function(x, ..., thresholds = 1e4L, right.cont = "both") {
  m <- x
  if (identical(m$functional$type, "prob")) {
    range_tt <- c(0, 1)
    xlim <- range_tt
  } else {
    range_tt <- range(m$x, m$y)
    xlim <- range_tt + c(-.05, .05) * (range_tt[2L] - range_tt[1L])
  }
  
  knots <- knots(m)
  msfun <- ms_fun(m)
  if (length(thresholds) == 1L && any(lengths(knots) > thresholds)) {
    stopifnot(thresholds > 2L)
    xx <- seq(xlim[1L], xlim[2L], length.out = thresholds)
    yy <- msfun(xx, right.cont)
    if (right.cont == "both") xx <- rep(xx, each = 2L)
    method <- rep(names(m$x), each = length(xx))
    rval <- data.frame(
      method = method,
      threshold = xx,
      score = c(yy),
      stringsAsFactors = FALSE
    )
  } else if (length(thresholds) > 1L) {
    xx <- sort(unique(thresholds))
    yy <- msfun(xx, right.cont)
    if (right.cont == "both") xx <- rep(xx, each = 2L)
    method <- rep(names(m$x), each = length(xx))
    rval <- data.frame(
      method = method,
      threshold = xx,
      score = c(yy),
      stringsAsFactors = FALSE
    )
  } else {
    yy0 <- msfun(xlim[1L], right.cont, FALSE)
    yy1 <- msfun("knots", right.cont, FALSE)
    yy2 <- msfun(xlim[2L], right.cont, FALSE)
    yy <- mapply(c, yy0, yy1, yy2, SIMPLIFY = FALSE)
    xx <- mapply(c, xlim[1L], knots, xlim[2L], SIMPLIFY = FALSE)
    if (right.cont == "both") xx <- mapply(rep, xx, each = 2L, SIMPLIFY = FALSE)
    rval <- do.call(
      rbind,
      lapply(
        seq_along(knots),
        function(i) {
          data.frame(
            method = names(m$x)[i],
            threshold = xx[[i]],
            score = yy[[i]],
            stringsAsFactors = FALSE
          )
        }
      )
    )
  }
  
  unique(rval)
}
