# creators for 'md_fun'-list in murphydiag objects

md_expect <- function(x, y, level) {
  coefs <- coef_expect(x, y, level)
  do.call(pwLinearFun, coefs)
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
  coefs <- coef_quant(x, y, level)
  do.call(pwConstantFun, coefs)
}