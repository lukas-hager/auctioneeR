#' The probability of no bids
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
#' @return The probability of no bids occurring in an auction
#' @export
#'
prob_no_bids <- function(v, j, n, mu_t, sigma_t, lambda_t, bids, default, phi, x_s, k){
  a <- (j-n)*log(stats::pnorm(x_s, mean = v, sd = sqrt(k)*sigma_t))
  e <- log(stats::dnorm(v, mean = mu_t, sd = sigma_t))
  exp(a+e)
}
