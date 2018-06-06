#' Examples
#' 
#' @param n Number of forecast observation pairs.
#' @param m Number of forecast methods.
#' 
#' @return A list.
#' 
#' @export
drawExample <- function(n = 500, m = 30) {
  information <- matrix(stats::rnorm(n * 10), n, 10)
  junk <- matrix(stats::rnorm(n * 10), n, 10)
  junk[, 3:4] <- junk[, 3:4] * ifelse(junk[, 3:4] > 0, 1.5, 1)
  junk[, 5:6] <- junk[, 5:6] * ifelse(junk[, 5:6] < 0, 1.5, 1)
  junk[, 7:8] <- abs(junk[, 7:8])
  junk[, 9:10] <- -abs(junk[, 9:10])
  
  information_sets <- replicate(
    m,
    list(inf = sort(sample(1:10, stats::rbinom(10, 10, .5))),
         jnk = sort(sample(1:10, sample(0:9, 1, prob = (10:1)^3)))),
    simplify = FALSE
  )
  
  y <- rowSums(information)
  x <- sapply(
    information_sets,
    function(ind) {
      rowSums(information[, ind$inf, drop = FALSE]) +
        rowSums(junk[, ind$jnk, drop = FALSE])
    }
  )
  
  list(x = x, y = y, sets = information_sets)
}




# # library(devtools)
# # install_github("aijordan/murphydiagram2")
# library(murphydiagram2)
# 
# n <- 1e3
# x1 <- rnorm(n)
# x2 <- rep(0, n)
# x3 <- -x1
# y <- x1 + rnorm(n)
# 
# m1 <- murphydiag(x1, y, "median", xnames = "m1")
# m2 <- murphydiag(x2, y, "median", xnames = "m2")
# m3 <- murphydiag(x3, y, "median", xnames = "m3")
# m <- c(m1, m2, m3)
# plot(m)
# 
# m1 <- murphydiag(x1, y, "mean", xnames = "m1")
# m2 <- murphydiag(x2, y, "mean", xnames = "m2")
# m3 <- murphydiag(x3, y, "mean", xnames = "m3")
# m <- c(m1, m2, m3)
# plot(m, lty = 1)
# 
# x1 <- pnorm(x1, lower.tail = FALSE)
# x2 <- pnorm(x2, lower.tail = FALSE)
# x3 <- pnorm(x3, lower.tail = FALSE)
# y <- rbinom(n, size = 1, prob = x1)
# m1 <- murphydiag(x1, y, "prob", xnames = "m1")
# m2 <- murphydiag(x2, y, "prob", xnames = "m2")
# m3 <- murphydiag(x3, y, "prob", xnames = "m3")
# m <- c(m1, m2, m3)
# plot(m, lty = 1)
# 
# n <- 1e3
# x1 <- rnorm(n)
# y <- x1 + rnorm(n)
# x2 <- lm(y ~ x1)
# 
# m1 <- murphydiag(x1, y, "mean", xnames = "x1")
# m2 <- murphydiag(x2)
# m <- c(m1, m2)
# plot(m, lty = 1)
# plot(c(m1, gpava(x1, y)$x))
# 
# #
# plot(murphydiag_diff(m1, m2))
# #
# plot(m1 - m2)
# #
# plot(murphydiag_diff(m))
# #
# plot(-m)
# 
# 
# ##########
# 
# n <- 1e2
# x1 <- rnorm(n)
# x2 <- rep(0, n)
# x3 <- -x1
# y <- rnorm(n) + x1
# 
# plot(c(murphydiag(x1, y, "mean"), gpava(x1, y)$x))
# plot(c(murphydiag(x1, y, "median"), gpava(x1, y, solver = weighted.median)$x))
# 
# x1 <- pnorm(x1, lower.tail = FALSE)
# x2 <- pnorm(x2, lower.tail = FALSE)
# x3 <- pnorm(x3, lower.tail = FALSE)
# y <- rbinom(n, size = 1, prob = x1)
# 
# m <- murphydiag(x1, y, "prob")
# m <- c(m, x2, x3, gpava(x1, y)$x, gpava(x2, y, ties = "secondary")$x, gpava(x3, y)$x)
# plot(m)