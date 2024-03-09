#' Generate The Optimal Probability-Weighted Bids
#'
#' This function expects that all arguments are adjusted to the units of the bid matrix.
#'
#' @param mu The auction's value of
#' @param sigma
#' @param c
#' @param lambda
#' @param n_max
#' @param k
#' @param mat
#' @param x_i
#'
#' @return
#' @export
#'
bid_vec <- function(
  mu,
  sigma,
  x_seq,
  c_seq,
  c_norm,
  lambda,
  n_max,
  k,
  mat
){
  p <- stats::dpois(2:n_max, lambda=lambda)
  f_y <- rbind(sapply(c(2:n_max), function(n_val){f_y_y(x_seq, mu, sigma, n_val, k)}))
  weights <- f_y %*% p

  bids <- rbind(
    sapply(
      2:n_max,
      function(n){
        akima::bicubic(c_seq, x_seq, mat[[n-1]], rep(c_norm, length(x_seq)), x_seq)$z
      }
    )
  )

  return(((bids * f_y) %*% p)/weights)
}
