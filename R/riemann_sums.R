#' Integrate via Riemann Sums
#'
#' @param f The function to be integrated
#' @param lower The lower bound of the integral
#' @param upper The upper bound of the integral
#' @param n_bins The number of bins to use for integration
#' @param ... Other arguments to be passed to the integrating function
#'
#' @return The area under the function
#' @export
#'
riemann_sums <- function(f, lower, upper, n_bins, ...){
  seq_vals = seq(lower, upper, length.out=n_bins)
  base_size = seq_vals[2]-seq_vals[1]
  sum(f(seq_vals, ...) * base_size)
}
