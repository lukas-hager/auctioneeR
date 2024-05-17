#' Compute the counterfactual for an auction under the assumption of resale
#'
#' @param c The default cost in the counterfactual
#' @param dt The datatable of simulations
#' @param bid_prep_cost The bid preparation cost for the auction
#' @param mu_t The auction's mean
#' @param sigma_t The auction's standard deviation
#' @param lambda_t The auction's value of lambda
#' @param r_t The auction's reserve price
#' @param k The coefficient on signal standard deviation
#' @param interps The interpolation grids
#' @param x_seq The sequence of signals
#' @param n_seq The possible number of bidders
#' @param f_weights The probability of being the high bidder
#'
#' @return A list of revenue to the seller and the equilibrium lambda for the auction
#' @export
#'
counterfactual_resale <- function(
    c, dt, bid_prep_cost, mu_t, sigma_t, lambda_t, r_t,
    k, interps, x_seq, n_seq, f_weights
){
  new_lambda <- uniroot(
    function(new_l){
      calc_bid_prep_cost(dt, mu_t, sigma_t, new_l, r_t, k, c, interps, x_seq, n_seq, f_weights) - bid_prep_cost
    },
    lower=1,
    upper=100
  )$root

  summ <- get_sim_summary(dt, mu_t, sigma_t, new_lambda, r_t, k, c, interps, x_seq, n_seq, f_weights)
  default_prob <- sum(dpois(c(1:30), new_lambda) * summ$defaults)
  mean_rev_no_default <- sum(dpois(c(1:30), new_lambda) * summ$mean_sale_rev)
  discount <- .97^(1/12)
  return(list('revenue' = ((1-default_prob)*mean_rev_no_default + default_prob * c)/(1-default_prob*discount),
              'lambda' = new_lambda))
}
