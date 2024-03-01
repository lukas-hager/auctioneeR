#' Calculate the CDF of the distribution of the posterior mean.
#'
#'
#' @param mu_1 The point on the distribution where the CDF is assessed.
#' @param mu_0 The prior mean.
#' @param sigma_0 The prior SD.
#' @param x_i The bidder's signal.
#' @param n The number of bidders.
#' @param k The coefficient on the signal standard variance.
#'
#' @return The CDF of the distribution of the expected posterior mean, evaluated at `mu_1`.
#' @export
#'

post_mean_cdf <- function(mu_1, mu_0, sigma_0, x_i, n, k){
  f = calc_f(n)
  s = sqrt(sigma_0^2*(1+k/(n-f)))

  transformed = h_inv(mu_1, mu_0, x_i, n, k)

  xi = (transformed-mu_0)/s
  beta = (x_i-mu_0)/s

  if (stats::pnorm(beta)>0){
    return(stats::pnorm(xi)*(transformed<=x_i)/stats::pnorm(beta) + (transformed>x_i))
  }else{
    return(0)
  }
}
