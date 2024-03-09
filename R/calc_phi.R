#' Calculate the Inverse Bidding Function
#'
#' @param mu_0 The prior value of the value mean.
#' @param sigma_0 The prior value of value SD.
#' @param mu_t The auction's value of the mean of the common value distribution.
#' @param sigma_t The auction's value of the standard deviation of the common value distribution.
#' @param n_max The maximum number of bidders in the auction.
#' @param k The coefficient on the signal standard deviation
#' @param x_seq The grid of signals.
#' @param c_seq The grid of default costs.
#' @param c_norm The auction's cost of default, normalized.
#' @param mat The interpolation matrices.
#' @param lambda_t The auction's value of the poisson parameter.
#'
#' @return An interpolation object for calculating the inverse of the bidding function.
#' @export
#'
calc_phi <- function(
    mu_t,
    sigma_t,
    mu_0,
    sigma_0,
    x_seq,
    c_seq,
    c_norm,
    lambda_t,
    n_max,
    k,
    mat
  ){
  a_1 <- sigma_t/sigma_0
  a_2 <- mu_t-mu_0*a_1

  bids <- bid_vec(
    mu_t,
    sigma_t,
    x_seq,
    c_seq,
    c_norm,
    lambda_t,
    n_max,
    k,
    mat
  )

  return(
    stats::splinefun(
      a_1*bids+a_2,
      a_1*x_seq+a_2,
      'natural'
    )
  )
}
