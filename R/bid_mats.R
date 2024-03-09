#' Calculates matrices of optimal bids for interpolation.
#'
#' @param x_seq Sequence of signals for which to compute bids for interpolation.
#' @param c_seq Sequence of default costs for which to compute bids for interpolation.
#' @param n_seq The possible numbers of bidders.
#' @param k The coefficient on the signal standard variance.
#' @param mu_0 The prior mean.
#' @param sigma_0 The prior SD.
#'
#' @return A list of bidding matrices for each set of parameters.
#' @export
#'

bid_mats <- function(x_seq, c_seq, n_seq, k, mu_0, sigma_0){
  bmat <- lapply(n_seq, function(n){matrix(nrow = length(c_seq), ncol = length(x_seq))})
  for (i in c(1:length(c_seq))){
    for (j in c(1:length(x_seq))){
      for (n in c(1:length(n_seq))){
        bmat[[n]][i,j] <- bid(x_seq[j], n_seq[n], c_seq[i], k, mu_0, sigma_0)
      }
    }
  }
  return(bmat)
}

