#' Calculate the Expected Value for a Given Signal
#'
#'
#' @param x The signal.
#' @param mu_0 The prior mean.
#' @param sigma_0 The prior SD.
#' @param k The coefficient on the signal standard variance.
#' @param n The number of bidders.
#'
#' @return The expectation of the value given signal `x`
#' @export

post_exp <- function(x, mu_0, sigma_0, n, k){
  sapply(x, function(x){
    stats::integrate(
      function(v){
        (n-1)*stats::dnorm(y, v, k*sigma_0)*stats::pnorm(y, v, k*sigma_0)^(n-2)*post_dist(y, mu_0, sigma_0, k)(v)
      },
      lower=-Inf,
      upper=Inf
    )$value
  })
}
