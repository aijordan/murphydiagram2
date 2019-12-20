#' Data frame approximations of Murphy diagrams
#' 
#' @param m an object inheriting from class \code{'murphydiag'}
#' @param window a numeric vector of the form \code{c(x1, x2, y1, y2)};
#'  e.g., \code{range(m)} or \code{par("usr")}
#' @param resolution a numeric vector of the form \code{c(nx, ny)}
#'  by which to divide \code{window}
#' @param thresholds
#'  a numeric vector of values where interpolation is to take place;
#'  alternatively, a single number specifying the amount of
#'  equally spaced points spanning \code{range(m, dim = "knots")}
#' 
#' @details 
#'  The primary purpose of \code{m_filter} and \code{m_approx}
#'  is to reduce the amount of points in \code{plot(m)}.
#'  
#'  \code{m_filter} aims to retain discontinuities and
#'  a visual impression that is close to the true empirical Murphy diagram.
#'  
#'  \code{m_approx} performs smooth linear interpolation between midpoints,
#'  evaluated at given threshold values on the x-axis.
#'  
#'  This is achieved by two different methods:
#'  
#'  \code{m_filter} moves sequentially through the rows
#'  of \code{as.data.frame(m)}. Any point with a value close to 0
#'  on the y-axis is retained. Otherwise,
#'  a rectangle with dimensions \code{c(x2 - x1, y2 - y1) / c(nx, ny)}
#'  is placed, with the latest retained point at its center.
#'  The next point that falls outside of this rectangle
#'  (and that point's predecessor) are then retained,
#'  and any points inbetween are discarded.
#'  
#'  \code{m_approx} performs a linear approximation
#'  on the data set of the knots and the midpoints
#'  of left-sided and right-sided limits
#'  of the Murphy diagram values in the respective knots.
#'  Afterwards that approximation is evaluated
#'  at user-specified thresholds or
#'  at a user-specified number of equally spaced points
#'  spanning \code{range(m, dim = "knots")}.
#' 
#' @return
#'  \code{m_filter} returns a \code{data.frame} with four columns: \code{name}, \code{knot}, \code{limit}, \code{value}. The returned \code{data.frame} comprises a subset of the rows returned by \code{as.data.frame(m)}.
#'  
#'  \code{m_approx} returns a \code{data.frame} with three columns: \code{name}, \code{threshold}, \code{value}
#' 
#' @seealso
#'  For an exact \code{data.frame} representation see \code{\link{as.data.frame.murphydiag}}.
#'  
#'  \code{\link{range.murphydiag}}
#' 
#' @name m_filter
NULL

#' @rdname m_filter
#' 
#' @export
m_filter <- function(m, window = range(m), resolution = 100) {
  stopifnot(is.murphydiag(m))
  stopifnot(is.numeric(window) && identical(length(window), 4L))
  stopifnot(is.numeric(resolution) && length(resolution) %in% 1:2)
  r <- c(window[2L] - window[1L], window[4L] - window[3L]) / resolution
  dflist <- lapply(seq_along(m), function(i) {
    df <- as.data.frame(m[i])
    df[C_thin_by(df$knot, df$value, r[1L], r[2L]), ]
  })
  do.call(rbind, dflist)
}

#' @rdname m_filter
#' 
#' @importFrom stats approx
#' 
#' @export
m_approx <- function(m, thresholds = 100) {
  stopifnot(is.murphydiag(m))
  if (identical(length(m), 0L)) return(data.frame())
  if (identical(length(attr(m, "y")), 0L)) return(data.frame())
  
  if (is.numeric(thresholds) && identical(length(thresholds), 1L)) {
    r <- range(m, dim = "knots")
    thresholds <- seq(r[1L], r[2L], length.out = thresholds)
  }
  
  stopifnot(is.numeric(thresholds) && length(thresholds) > 1L)
  
  dflist <- lapply(seq_along(m), function(i) {
    d <- m[[i]]
    data.frame(stringsAsFactors = FALSE,
      name = names(m)[i],
      threshold = thresholds,
      value = approx(
        x = d$knots,
        y = 0.5 * (d$values$left + d$values$right),
        xout = thresholds,
        yleft = 0, yright = 0
      )$y
    )
  })
  do.call(rbind, dflist)
}

eval_m_at <- function(m, thresholds = NULL) {
  stopifnot(is.murphydiag(m))
  if (identical(length(thresholds), 0L)) return(as.data.frame(m))
  if (identical(length(m), 0L)) return(data.frame())
  if (identical(length(attr(m, "y")), 0L)) return(data.frame())
  
  if (identical(length(thresholds), 1L) && is.atomic(thresholds)) {
    r <- range(attr(m, "y"), sapply(m, function(d) range(d$x)))
    thresholds <- seq(r[1], r[2], length.out = thresholds)
  }
  
  stopifnot(is.numeric(thresholds) && length(thresholds) > 1L)
  stopifnot(!is.unsorted(thresholds))
  
  df_list <- lapply(seq_along(m), function(i) {
    d <- m[[i]]
    idx <- findInterval(thresholds, d$knots)
    val <- numeric(length(thresholds))
    imin <- max(1L, which.max(idx > 0L))
    imax <- -max(1L, which.max(rev(idx) < length(d$knots))) + length(idx) + 1L
    if (imin < imax) {
      idx <- idx[imin:imax]
      tt <- thresholds[imin:imax]
      val[imin:imax] <-
        (d$values$right[idx] * (d$knots[idx + 1L] - tt) +
           d$values$left[idx + 1L] * (tt - d$knots[idx])) /
        (d$knots[idx + 1L] - d$knots[idx])
    }
    data.frame(stringsAsFactors = FALSE,
      name = names(m)[i],
      threshold = thresholds,
      value = val
    )
  })
  do.call(rbind, df_list)
}

attributes_without_names <- function(m) {
  attributes(m)[names(attributes(m)) != "names"]
}

#########################################################################
##  DEPRECATED

# as.data.frame.murphydiag <- function(x, ..., thresholds = 1e4L, right.cont = "both") {
#   m <- x
#   if (identical(m$functional$type, "prob")) {
#     range_tt <- c(0, 1)
#     xlim <- range_tt
#   } else {
#     range_tt <- range(m$x, m$y)
#     xlim <- range_tt + c(-.05, .05) * (range_tt[2L] - range_tt[1L])
#   }
#   
#   knots <- knots(m)
#   msfun <- ms_fun(m)
#   if (length(thresholds) == 1L && any(lengths(knots) > thresholds)) {
#     stopifnot(thresholds > 2L)
#     xx <- seq(xlim[1L], xlim[2L], length.out = thresholds)
#     yy <- msfun(xx, right.cont)
#     if (right.cont == "both") xx <- rep(xx, each = 2L)
#     method <- rep(names(m$x), each = length(xx))
#     rval <- data.frame(
#       method = method,
#       threshold = xx,
#       score = c(yy),
#       stringsAsFactors = FALSE
#     )
#   } else if (length(thresholds) > 1L) {
#     xx <- sort(unique(thresholds))
#     yy <- msfun(xx, right.cont)
#     if (right.cont == "both") xx <- rep(xx, each = 2L)
#     method <- rep(names(m$x), each = length(xx))
#     rval <- data.frame(
#       method = method,
#       threshold = xx,
#       score = c(yy),
#       stringsAsFactors = FALSE
#     )
#   } else {
#     yy0 <- msfun(xlim[1L], right.cont, FALSE)
#     yy1 <- msfun("knots", right.cont, FALSE)
#     yy2 <- msfun(xlim[2L], right.cont, FALSE)
#     yy <- mapply(c, yy0, yy1, yy2, SIMPLIFY = FALSE)
#     xx <- mapply(c, xlim[1L], knots, xlim[2L], SIMPLIFY = FALSE)
#     if (right.cont == "both") xx <- mapply(rep, xx, each = 2L, SIMPLIFY = FALSE)
#     rval <- do.call(
#       rbind,
#       lapply(
#         seq_along(knots),
#         function(i) {
#           data.frame(
#             method = names(m$x)[i],
#             threshold = xx[[i]],
#             score = yy[[i]],
#             stringsAsFactors = FALSE
#           )
#         }
#       )
#     )
#   }
#   
#   unique(rval)
# }

# creators for piece-wise functions
pwPolyFun <- function(knots, ...) {
  coefs <- data.frame(...)
  function(x, right.cont = TRUE, coef = FALSE) {
    
    if (isTRUE(all.equal(x, "knots"))) {
      if (right.cont == "both") {
        times <- rep.int(c(1L, 2L, 1L), c(1L, length(knots) - 1L, 1L))
        coefs <- as.data.frame(lapply(coefs, rep.int, times = times))
        if (!coef) x <- rep(knots, each = 2L)
      } else {
        ind <- ifelse(right.cont, 1L, nrow(coefs))
        coefs <- coefs[-ind, , drop = FALSE]
        if (!coef) x <- knots
      }
    } else {
      if (right.cont == "both") {
        n <- length(x) * 2L
        ind <- numeric(n)
        ind[seq.int(1, n, 2L)] <- findInterval(x, knots, left.open = TRUE) + 1L
        ind[seq.int(2, n, 2L)] <- findInterval(x, knots, left.open = FALSE) + 1L
        coefs <- coefs[ind, , drop = FALSE]
        if (!coef) x <- rep(x, each = 2L)
      } else {
        ind <- findInterval(x, knots, left.open = !right.cont) + 1L
        coefs <- coefs[ind, , drop = FALSE]
      }
    }
    
    if (coef) {
      as.matrix(coefs)
    } else {
      if (length(coefs) == 1L) {
        coefs[[1L]]
      } else if (length(coefs) == 2L) {
        coefs[[1L]] + coefs[[2L]] * x
      } else {
        last <- length(coefs)
        res <- coefs[[last]]
        for (i in (last - 1L):1L) {
          res <- res * x + coefs[[i]]
        }
        res
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
  pwPolyFun(knots, do.call(`-`, a))
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
  pwPolyFun(knots, do.call(`-`, a), do.call(`-`, b))
}


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
    coefs <- coef_sort(knots, a = a, b = b, d = d, e = e, f = f)
    coefs <- do.call(coef_cumsum, coefs)
    coefs <- do.call(coef_unique, coefs)
    coefs <- do.call(coef_append0, coefs)
    coefs$d <- coefs$d - coefs$a^2
    coefs$e <- coefs$e - coefs$a * coefs$b
    coefs$f <- coefs$f - coefs$b^2
    coefs
  } else {
    coefs <- coef_sort(knots, a = a, b = b)
    coefs <- do.call(coef_cumsum, coefs)
    coefs <- do.call(coef_unique, coefs)
    do.call(coef_append0, coefs)
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
    coefs <- coef_sort(knots, a = a, d = d)
    coefs <- do.call(coef_cumsum, coefs)
    coefs <- do.call(coef_unique, coefs)
    coefs <- do.call(coef_append0, coefs)
    coefs$d <- coefs$d - coefs$a^2
    coefs
  } else {
    coefs <- coef_sort(knots, a = a)
    coefs <- do.call(coef_cumsum, coefs)
    coefs <- do.call(coef_unique, coefs)
    do.call(coef_append0, coefs)
  }
}


coef_sort <- function(knots, ...) {
  tmp <- order(knots)
  knots <- knots[tmp]
  params <- lapply(
    list(...),
    function(x) x[tmp]
  )
  c(knots = list(knots), params)
}

coef_cumsum <- function(knots, ...) {
  params <- lapply(
    list(...),
    cumsum
  )
  c(knots = list(knots), params)
}

coef_unique <- function(knots, ...) {
  tmp <- duplicated(knots, fromLast = TRUE)
  knots <- knots[!tmp]
  params <- lapply(
    list(...),
    function(x) x[!tmp]
  )
  c(knots = list(knots), params)
}

coef_append0 <- function(knots, ...) {
  params <- lapply(
    list(...),
    function(x) c(0, x)
  )
  c(knots = list(knots), params)
}

coef_diff <- function(m) {
  knots <- switch(
    m$functional$type,
    mean =,
    prob = c(m$x, recursive = TRUE, use.names = FALSE),
    median =,
    quantile =,
    expectile = c(m$x, m$y, recursive = TRUE, use.names = FALSE))
  knots <- sort(unique(knots))
  
  params <- lapply(
    m$md_fun,
    function(FUN) FUN(knots, coef = TRUE)
  )
  
  coefs <- c(knots = list(knots), as.data.frame(params[[1L]] - params[[2L]]))
  do.call(coef_append0, coefs)
}

# Function to compute variance estimator which accounts for autocorrelation
hac_sd <- function(x, k = 0){
  
  u <- x - mean(x)
  n <- length(u)
  
  # choose nr of lags (if not provided)
  if (is.null(k)){
    
    aux <- lm(u[-1]~u[-n]-1)
    rho <- unname(aux$coefficients)
    sigma <- sum(unname(aux$residuals)^2) / n
    
    top <- sum( (4*(rho^2) * (sigma^2)) / (((1-rho)^6)*((1+rho)^2)) )
    bot <- sum( (sigma^2) / ((1-rho)^4) )
    k <- min(c(ceiling(1.1447*((top/bot)*n)^(1/3)), round(0.5*n)))
    
  }
  
  # compute HAC
  vcv <- sum(u^2) / n
  
  # If k > 0: Add autocovariance terms
  if (k > 0){
    w <- 1 - (1:k)/(k+1)
    for (i in 1:k){
      cov <- sum(u[(i+1):n] * u[1:(n-i)]) / n
      vcv <- vcv + 2*w[i]*cov
    }
  }
  
  sqrt(vcv/n)
  
}

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
  lapply(Fn, function(x) x$knots)
}

intercepts <- function(Fn, ...) {
  lapply(Fn$md_fun, function(FUN) eval(expression(coefs$a), environment(FUN)))
}

slopes <- function(Fn, ...) {
  lapply(Fn$md_fun, function(FUN) {
    eval(expression(tryCatch(coefs$b, error = function(e) NULL)), environment(FUN))
  })
}


# creators for 'md_fun'-list in murphydiag objects

md_expect <- function(x, y, level) {
  c(list(x = x), C_md_expect(x, y, level, order(y)))
  # coefs <- coef_expect(x, y, level)
  # do.call(pwPolyFun, coefs)
}

md_mean <- function(x, y)
  md_expect(x, y, 0.5)

md_median <- function(x, y)
  md_quant(x, y, 0.5)

md_prob <- function(x, y) {
  md_expect(x, y, 0.5)
}

md_quant <- function(x, y, level) {
  coefs <- coef_quant(x, y, level)
  do.call(pwPolyFun, coefs)
}

md_diff <- function(m) {
  coefs <- coef_diff(m)
  do.call(pwPolyFun, coefs)
}


###
es_fun <- function(m) {
  stopifnot(inherits(m, "murphydiag"))
  switch(m$functional$type,
         mean = es_mean_fun(m$x, m$y),
         prob = es_prob_fun(m$x, m$y),
         median = es_median_fun(m$x, m$y),
         quantile = es_quant_fun(m$x, m$y, m$functional$level),
         expectile = es_expect_fun(m$x, m$y, m$functional$level))
}

###
ms_fun <- function(m) {
  stopifnot(inherits(m, "murphydiag"))
  function(t, right.cont = TRUE, simplify = TRUE) {
    args <- list(t, right.cont)
    sapply(
      m$md_fun,
      function(FUN) do.call(FUN, args),
      simplify = simplify
    )
  }
}
#   switch(m$functional$type,
#          mean = ms_mean_fun(m$x, m$y),
#          prob = ms_prob_fun(m$x, m$y),
#          median = ms_median_fun(m$x, m$y),
#          quantile = ms_quant_fun(m$x, m$y, m$functional$level),
#          expectile = ms_expect_fun(m$x, m$y, m$functional$level))
# }


