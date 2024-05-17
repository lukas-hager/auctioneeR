#' Calculate the bid preparation cost for an auction
#'
#' @param dt Simulated dataframe
#' @param mu_t Auction's mean
#' @param sigma_t Auction's SD
#' @param lambda_t Auction's lambda parameter
#' @param r_t Auction's reserve
#' @param k Signal standard deviation coefficient
#' @param c Cost of default
#' @param interps The interpolations for bidding
#' @param x_seq The sequence of signals
#' @param n_seq The possible number of bidders
#' @param f_weights The probability of being the high bidder
#'
#' @return The bid preparation cost for an auction
#' @export
#'
calc_bid_prep_cost <- function(dt, mu_t, sigma_t, lambda_t, r_t, k, c, interps, x_seq, n_seq, f_weights){
  summ <- get_sim_summary(dt, mu_t, sigma_t, lambda_t, r_t, k, c, interps, x_seq, n_seq, f_weights)
  sum(dpois(c(1:30), lambda_t) * summ$mean_payoff/c(1:30))
}
