#' Calculate the probability of default given bids.
#'
#' @param v The value
#' @param mu_t The prior on the mean of the value distribution
#' @param sigma_t The prior on the SD of the value distribution
#' @param lambda_t The entry probability parameter
#' @param bids The bids
#' @param phi The inverse bid function
#' @param default Whether or not default occurred
#' @param x_s The cutoff signal
#' @param c The cost of default
#' @param k The multiplier on the SD of the bidder's signals
#' @param n_max The max number of bidders
#' @param dr The base rate default probability
#'
#' @return A default probability for a set of bids
#' @export
#'
prob_default_given_bids <- function(v, mu_t, sigma_t, lambda_t, bids, phi, default, x_s, c, k, n_max, dr){
  dp <- min(default_prob(v, mu_t, sigma_t, lambda_t, bids, phi, x_s, c, k, n_max) + dr, 1)
  d <- log(max(default * dp + (1-default) * (1-dp),0))
  e <- log(stats::dnorm(v, mean = mu_t, sd = sigma_t))
  exp(d+e)
}
