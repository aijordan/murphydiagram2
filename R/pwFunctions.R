# creators for piece-wise functions

pwConstantFun <- function(knots, a) {
  function(x, right.cont = TRUE) {
    if (isTRUE(all.equal(x, "knots"))) {
      if (right.cont == "both") {
        rep.int(a, rep.int(c(1L, 2L, 1L), c(1L, length(a) - 2L, 1L)))
      } else if (right.cont == TRUE) {
        a[-1L]
      } else if (right.cont == FALSE) {
        a[-length(a)]
      }
    } else {
      if (right.cont == "both") {
        n <- length(x)
        ind <- numeric(2L * n)
        ind[2L * 1:n - 1] <- findInterval(x, knots, left.open = TRUE) + 1L
        ind[2L * 1:n] <- findInterval(x, knots, left.open = FALSE) + 1
        a[ind]
      } else {
        a[findInterval(x, knots, left.open = !right.cont) + 1L]
      }
    }
  }
}

pwLinearFun <- function(knots, a, b) {
  function(x, right.cont = TRUE) {
    if (isTRUE(all.equal(x, "knots"))) {
      if (right.cont == "both") {
        times <- rep.int(c(1L, 2L, 1L), c(1L, length(a) - 2L, 1L))
        rep.int(a, times) + rep.int(b, times) * rep(knots, each = 2L)
      } else if (right.cont == TRUE) {
        a[-1L] + b[-1L] * knots
      } else if (right.cont == FALSE) {
        a[-length(a)] + b[-length(b)] * knots
      }
    } else {
      if (right.cont == "both") {
        n <- length(x)
        ind <- numeric(2L * n)
        ind[2L * 1:n - 1] <- findInterval(x, knots, left.open = TRUE) + 1L
        ind[2L * 1:n] <- findInterval(x, knots, left.open = FALSE) + 1
        a[ind] + b[ind] * rep(x, each = 2L)
      } else {
        ind <- findInterval(x, knots, left.open = !right.cont) + 1L
        a[ind] + b[ind] * x
      }
    }
  }
}

#

pwConstantFun_diff <- function(m) {
  knots <- sort(unique(c(m$x, m$y, recursive = TRUE, use.names = FALSE)))
  knots_old <- knots(m)
  a <- intercepts(m)
  a <- lapply(
    seq_along(a),
    function(i) a[[i]][c(1L, findInterval(knots, knots_old[[i]]) + 1L)]
  )
  pwConstantFun(knots, do.call(`-`, a))
}

pwLinearFun_diff <- function(m) {
  knots <- switch(
    m$functional$type,
    mean =,
    prob = c(m$x, recursive = TRUE, use.names = FALSE),
    expectile = c(m$x, m$y, recursive = TRUE, use.names = FALSE))
  knots <- sort(unique(knots))
  knots_old <- knots(m)
  a <- intercepts(m)
  b <- slopes(m)
  ind <- lapply(knots_old, function(k) c(1L, findInterval(knots, k) + 1L))
  a <- lapply(seq_along(a), function(i) a[[i]][ind[[i]]])
  b <- lapply(seq_along(b), function(i) b[[i]][ind[[i]]])
  pwLinearFun(knots, do.call(`-`, a), do.call(`-`, b))
}
