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
  # the posterior distribution of the sample mean after having seen one's signal
  s = sqrt(sigma_0^2*((n-1)*k + k*(k-1))/((n-1)*(k+1)))
  m = (k * mu_0 + x_i)/(k+1)

  # putting the desired value of the posterior mean in terms of sample mean
  transformed = h_inv(mu_1, mu_0, x_i, n, k)

  xi = (transformed-m)/s
  beta = (x_i-m)/s

  if (stats::pnorm(beta)>0){
    return(stats::pnorm(xi)*(transformed<=x_i)/stats::pnorm(beta) + (transformed>x_i))
  }else{
    return(0)
  }
}
