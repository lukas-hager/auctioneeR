#' Calculate the expectation of the distribution of the posterior mean.
#'
#'
#' @param x_i The bidder's signal.
#' @param mu_0 The prior mean.
#' @param sigma_0 The prior SD.
#' @param n The number of bidders.
#' @param k The coefficient on the signal standard variance.
#' @param lb The lower truncation of the distribution (assumed to not exist if not specified).
#'
#' @return The expectation of the posterior mean, given that it's less than the bidder's signal.
#' @export
#'

post_mean_exp <- function(x_i, mu_0, sigma_0, n, k, lb=-Inf){
  f = calc_f(n)
  s = sqrt(sigma_0^2*(1+k/(n-f)))

  beta = (x_i-mu_0)/s
  alpha = (h_inv(lb, mu_0, x_i, n, k)-mu_0)/s

  ifelse(
    ((stats::pnorm(beta)-stats::pnorm(alpha))>0) & (!is.na(stats::pnorm(beta)-stats::pnorm(alpha))),
    (k*mu_0+f*x_i)/(n+k) + ((n-f)/(n+k)) * (mu_0 + (stats::dnorm(alpha)-stats::dnorm(beta))*s/(stats::pnorm(beta)-stats::pnorm(alpha))),
    -1e6
  )
}
