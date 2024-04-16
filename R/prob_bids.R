#' The probability of observing bids
#'
#' @param v The value of the item
#' @param j The total number of bidders
#' @param n The number of observed bidders
#' @param mu_t The mean of the prior distribution
#' @param sigma_t The standard deviation of the prior distribution
#' @param lambda_t The parameter governing the poisson entry process
#' @param bids The bids
#' @param default Whether or not default occurred
#' @param phi The inverse bidding function
#' @param x_s The cutoff signal
#' @param k The multiplier on the signal standard deviation
#'
#' @return The probability of observing the bids that occurred
#' @export
#'
prob_bids <- function(v, j, n, mu_t, sigma_t, lambda_t, bids, default, phi, x_s, k){
  # dp <- default_prob(v, mu_t, sigma_t, bids, phi, signals)
  a <- (j-n)*log(stats::pnorm(x_s, mean = v, sd = sqrt(k)*sigma_t))
  b <- log(1-stats::pnorm(phi(bids[1]), mean = v, sd = sqrt(k)*sigma_t))
  c <-ifelse(
    rep(n>1, length(v)),
    sapply(v, function(v){sum(log(stats::dnorm(phi(bids[-1]), mean = v, sd = sqrt(k)*sigma_t)*phi(bids[-1], deriv=1)))}),
    rep(log(1), length(v))
  )
  # d <- log(default * dp + (1-default) * (1-dp))
  e <- log(stats::dnorm(v, mean = mu_t, sd = sigma_t))
  exp(a+b+c+e)
}
