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
  # the posterior distribution of the sample mean after having seen one's signal
  s = sqrt(sigma_0^2*((n-1)*k + k*(k-1))/((n-1)*(k+1)))
  m = (k * mu_0 + x_i)/(k+1)

  beta = (x_i-m)/s
  alpha = (h_inv(lb, m, x_i, n, k)-m)/s

  ifelse(
    ((stats::pnorm(beta)-stats::pnorm(alpha))>0) & (!is.na(stats::pnorm(beta)-stats::pnorm(alpha))),
    (k*m+x_i)/(n+k) + ((n-1)/(n+k)) * (m + (stats::dnorm(alpha)-stats::dnorm(beta))*s/(stats::pnorm(beta)-stats::pnorm(alpha))),
    -1e6
  )
}
