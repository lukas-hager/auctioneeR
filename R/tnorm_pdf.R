#' Calculate the PDF of a Normal Truncated Above
#'
#'
#' @param x The point where we assess the PDF.
#' @param mu The distribution's mean.
#' @param sigma The distribution's SD.
#' @param k The coefficient on the signal standard variance.
#' @param ub The upper truncation of the normal.
#'
#' @return The PDF at `x`
#' @export

tnorm_pdf <- function(x, mu, sigma, k, ub){
  z <- (x-mu)/(k*sigma)
  stats::dnorm(z)*(x<=ub)/(k*sigma*stats::pnorm((ub-mu)/(k*sigma)))
}
