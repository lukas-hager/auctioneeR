#' Calculate the CDF of a Normal Truncated Above
#'
#'
#' @param x The point where we assess the CDF.
#' @param mu The distribution's mean.
#' @param sigma The distribution's SD.
#' @param k The coefficient on the signal standard variance.
#' @param ub The upper truncation of the normal.
#'
#' @return The CDF at `x`
#' @export

tnorm_cdf <- function(x, mu, sigma, k, ub){
  z <- (x-mu)/(k*sigma)
  ifelse(x<ub, stats::pnorm(z)/stats::pnorm((ub-mu)/(k*sigma)),1)
}
