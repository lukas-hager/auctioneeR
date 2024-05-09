#' Calculates matrices of optimal bids for interpolation.
#'
#' @param n_max The maximum number of bidders in the auction.
#' @param k The coefficient on the signal standard variance.
#' @param x_seq The grid of signal values
#' @param c_seq The grid of default cost values
#'
#' @return A list of bidding matrices for each set of parameters.
#' @export
#'

bid_mats <- function(k, n_max, x_seq, c_seq){
  diff_vals <- seq(-4, 0, length.out=20)

  # create interpolation grids for beliefs about default
  interp_grids <- create_interp_mats(k, n_max, x_seq, diff_vals)

  # solve the normalized auction for a grid of signals and costs, as well as for
  # all possible number of bidders
  n_seq <- c(2:n_max)
  bmat <- lapply(n_seq, function(n){matrix(nrow = length(c_seq), ncol = length(x_seq))})
  for (n_val in c(1:length(n_seq))){
    for (j_val in c(1:length(x_seq))){
      interps <- get_moments(x_seq[j_val], n_seq[n_val], interp_grids, x_seq, diff_vals)
      for (i_val in c(1:length(c_seq))){
        bmat[[n_val]][i_val,j_val] <- bid(x_seq[j_val], n_seq[n_val], c_seq[i_val], k, interps)
      }
    }
  }
  return(bmat)
}

