#' Probability of a sample mean given a signal
#'
#' @param v The value of the item
#' @param x_bar The sample mean of the signals
#' @param x_i The bidder's signal
#' @param n_signals The number of bidders
#' @param k The coefficient on the signal standard deviation
#'
#' @return The probability of a sample mean given a signal
#' @export
#'
f = function(v, x_bar, x_i, n_signals,k){
  sigma <- sqrt(k)
  beta <- (x_i-v)/(sigma)
  mu_trunc <- v - sqrt(k)*sigma*(stats::dnorm(beta)/stats::pnorm(beta))
  sig_trunc <- sqrt(k)*sigma * sqrt(1-beta*stats::dnorm(beta)/stats::pnorm(beta) - (stats::dnorm(beta)/stats::pnorm(beta))^2)
  stats::dnorm(v)*stats::dnorm(beta)*stats::pnorm(beta)^(n_signals-1)*stats::dnorm(x_bar, mu_trunc, sig_trunc/sqrt(n_signals))
}
