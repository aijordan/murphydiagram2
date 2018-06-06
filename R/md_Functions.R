# creators for 'md_fun'-list in murphydiag objects

md_expect <- function(x, y, level) {
  yltx <- y < x
  knots <- c(
    ifelse(yltx, y, x),
    ifelse(yltx, x, y)
  )
  asym <- 4 * (yltx - level) / length(yltx)
  a <- -asym * y
  a <- c(a, -a)
  b <- c(asym, -asym)
  
  tmp <- order(knots)
  knots <- knots[tmp]
  a <- cumsum(a[tmp])
  b <- cumsum(b[tmp])
  
  tmp <- duplicated(knots, fromLast = TRUE)
  knots <- knots[!tmp]
  a <- c(0, a[!tmp])
  b <- c(0, b[!tmp])
  
  pwLinearFun(knots, a, b)
}

md_mean <- function(x, y)
  md_expect(x, y, 0.5)

md_median <- function(x, y)
  md_quant(x, y, 0.5)

md_prob <- function(x, y) {
  x[x < 0 | x > 1] <- NaN
  y[y != 0 & y != 1] <- NaN
  md_expect(x, y, 0.5)
}

md_quant <- function(x, y, level) {
  yltx <- y < x
  knots <- c(
    ifelse(yltx, y, x),
    ifelse(yltx, x, y)
  )
  a <- 2 * abs(yltx - level) / length(yltx)
  a <- c(a, -a)
  
  tmp <- order(knots)
  knots <- knots[tmp]
  a <- cumsum(a[tmp])
  
  tmp <- duplicated(knots, fromLast = TRUE)
  knots <- knots[!tmp]
  a <- c(0, a[!tmp])
  
  pwConstantFun(knots, a)
}
