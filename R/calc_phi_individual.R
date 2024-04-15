#' Calculate the Inverse Bidding Function
#'
#' TODO: Not completed
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
calc_phi_individual<- function(
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
  x_seq <- seq(mu_t-5*sigma_t, mu_t+5*sigma_t, length.out = 100)
  p <- stats::dpois(2:n_max, lambda=lambda_t)
  f_y <- rbind(sapply(c(2:n_max), function(n_val){f_y_y(x_seq, mu_t, sigma_t, n_val, k)}))
  weights <- f_y %*% p

  bids <- rbind(
    sapply(
      2:n_max,
      function(n){
        bid(x_i,n,c,k,mu_0,sigma_0)
      }
    )
  )

  return(
    stats::splinefun(
      bids,
      x_seq,
      'natural'
    )
  )
}
