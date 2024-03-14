#' Generate The Optimal Probability-Weighted Bids
#'
#' This function expects that all arguments are adjusted to the units of the bid matrix.
#'
#' @param mu_t The auction's value of the mean of the common value distribution.
#' @param sigma_t The auction's value of the standard deviation of the common value distribution.
#' @param n_max The maximum number of bidders in the auction.
#' @param k The coefficient on the signal standard deviation
#' @param x_seq The grid of signals.
#' @param c_seq The grid of default costs.
#' @param c The auction's nominal cost of default
#' @param mat The interpolation matrices.
#' @param lambda_t The auction's value of the poisson parameter.
#' @param mu_base The mean of the base case auction.
#' @param sigma_base The SD of the base case auction.
#'
#' @return Returns a vector of normalized bids for the auction's normalized cost of default.
#' @export
#'
bid_vec <- function(
  mu_t,
  sigma_t,
  x_seq,
  c_seq,
  c,
  lambda_t,
  n_max,
  k,
  mat,
  mu_base=0,
  sigma_base=1
){
  a_1 <- sigma_t
  a_2 <- mu_t

  p <- stats::dpois(2:n_max, lambda=lambda_t)
  f_y <- rbind(sapply(c(2:n_max), function(n_val){f_y_y(x_seq, mu_t, sigma_t, n_val, k)}))
  weights <- f_y %*% p

  bids <- rbind(
    sapply(
      2:n_max,
      function(n){
        akima::bicubic(c_seq, x_seq, mat[[n-1]], rep(c / a_1, length(x_seq)), x_seq)$z
      }
    )
  )

  return(as.vector(a_1 * ((bids * f_y) %*% p)/weights + a_2))
}
