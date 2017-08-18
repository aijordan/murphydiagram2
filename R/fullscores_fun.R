#' @rdname scores_fun
#' @export fs_fun
fs_fun <- function(m) {
  stopifnot(inherits(m, "murphydiag"))
  switch(m$functional$type,
         mean = fs_mean_fun(m$x, m$y),
         prob = fs_prob_fun(m$x, m$y),
         median = fs_median_fun(m$x, m$y),
         quantile = fs_quant_fun(m$x, m$y, m$functional$level),
         expectile = fs_expect_fun(m$x, m$y, m$functional$level))
}


#' @rdname scores_fun
#' @export fs_mean_fun
fs_mean_fun <- function(x, y)
  function(t, right = FALSE) fs_mean(t, x, y, right)


#' @rdname scores_fun
#' @export fs_prob_fun
fs_prob_fun <- function(x, y)
  function(t, right = FALSE) fs_prob(t, x, y, right)


#' @rdname scores_fun
#' @export fs_median_fun
fs_median_fun <- function(x, y)
  function(t, right = FALSE) fs_median(t, x, y, right)


#' @rdname scores_fun
#' @export fs_quant_fun
fs_quant_fun <- function(x, y, level)
  function(t, right = FALSE) fs_quant(t, x, y, level, right)


#' @rdname scores_fun
#' @export fs_expect_fun
fs_expect_fun <- function(x, y, level)
  function(t, right = FALSE) fs_expect(t, x, y, level, right)
