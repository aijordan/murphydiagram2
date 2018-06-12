coef_expect <- function(x, y, level, diff = FALSE, with_sd = FALSE) {
  if (!diff) {
    yltx <- y < x
    knots <- as.vector(t(apply(cbind(x, y), 1L, sort)))
    asym <- 4 * (yltx - level)
    pre_ab <- c(asym, -asym) / length(y)
    a <- -y * pre_ab
    b <- pre_ab
    if (with_sd) {
      pre_def <- c(asym^2, -asym^2) / length(y)
      d <- y^2 * pre_def
      e <- -y * pre_def
      f <- pre_def
    }
  } else if (diff) {
    x <- as.matrix(x)
    xltx <- x[, 1L] < x[, 2L]
    rnk <- apply(y > x, 1, sum)
    knots <- as.vector(t(apply(cbind(x, y), 1L, sort)))
    asym1 <- ifelse(rnk == 0L, 0, 4 * (  - level))
    asym2 <- ifelse(rnk == 2L, 0, 4 * (1 - level))
    pre_ab <- c(asym1, -asym1 - asym2, asym2) * ifelse(xltx, 1, -1) / length(y)
    a <- -y * pre_ab
    b <- pre_ab
    if (with_sd) {
      pre_def <- c(asym1^2, -asym1^2 + asym2^2, -asym2^2) / length(y)
      d <- y^2 * pre_def
      e <- -y * pre_def
      f <- pre_def
    }
  }
  
  if (with_sd) {
    coef_aggregate(knots, a = a, b = b, d = d, e = e, f = f)
  } else {
    coef_aggregate(knots, a = a, b = b)
  }
}


coef_quant <- function(x, y, level, diff = FALSE, with_sd = FALSE) {
  if (!diff) {
    yltx <- y < x
    knots <- as.vector(t(apply(cbind(x, y), 1L, sort)))
    asym <- 2 * abs(yltx - level)
    a <- c(asym, -asym) / length(y)
    if (with_sd) {
      d <- c(asym^2, -asym^2) / length(y)
    }
  } else if (diff) {
    x <- as.matrix(x)
    xltx <- x[, 1L] < x[, 2L]
    rnk <- apply(y > x, 1, sum)
    knots <- as.vector(t(apply(cbind(x, y), 1L, sort)))
    asym1 <- ifelse(rnk == 0L, 0, 2 * abs(  - level))
    asym2 <- ifelse(rnk == 2L, 0, 2 * abs(1 - level))
    a <- c(asym1, -asym1 - asym2, asym2) / length(y)
    if (with_sd) {
      d <- c(asym1^2, -asym1^2 + asym2^2, -asym2^2) / length(y)
    }
  }
  
  if (with_sd) {
    coef_aggregate(knots, a = a, d = d)
  } else {
    coef_aggregate(knots, a = a)
  }
}


coef_aggregate <- function(knots, ...) {
  tmp <- order(knots)
  knots <- knots[tmp]
  params <- lapply(
    list(...),
    function(x) cumsum(x[tmp])
  )
  
  tmp <- duplicated(knots, fromLast = TRUE)
  knots <- knots[!tmp]
  params <- lapply(
    params,
    function(x) c(0, x[!tmp])
  )
  
  if ("d" %in% names(params)) params$d <- params$d - params$a^2
  if ("e" %in% names(params)) params$e <- params$e - params$a * params$b
  if ("f" %in% names(params)) params$f <- params$f - params$b^2
  
  c(list(knots = knots), params)
}

