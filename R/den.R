#' The denominator of the expected value expression
#'
#' @param v The value of the object
#' @param x The signal
#' @param n The number of bidders
#' @param mu_0 The prior mean of the value distribution
#' @param sigma_0 The prior SD of the value distribution
#' @param k The multiplier on the SD of the signals
#'
#' @return The probability of being the highest bidder
#' @export
#'
den <- function(v, x, n, mu_0, sigma_0, k){
  # f_v <- post_dist(x,mu_0,sigma_0,k,fun='pdf')
  f_v <- stats::dnorm(v, mean=mu_0, sd=sigma_0)
  f_x <- stats::dnorm(x, mean=v, sd = sqrt(k)*sigma_0)
  F_x <- stats::pnorm(x, mean=v, sd = sqrt(k)*sigma_0)
  return(f_v * f_x * F_x^(n-1))
}
