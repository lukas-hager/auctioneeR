#' Equilibrium Function
#'
#' This function will take value zero for the equilibrium bid `b`.
#'
#'
#' @param b The proposed bid.
#' @param x_i The bidder's signal.
#' @param n The number of bidders.
#' @param c The cost of default.
#' @param k The coefficient on the signal standard variance.
#'
#' @return The error of the equilibrium function at bid `b`.
#' @export
#'

eqm <- function(b, mu_0, sigma_0, x_i, n, c, k){
  p_default <- post_mean_cdf(b-c, mu_0, sigma_0, x_i, n, k)
  p_default*c + (1-p_default)*b - (1-p_default) * post_mean_exp(x_i, mu_0, sigma_0, n, k, lb=b-c)
}
