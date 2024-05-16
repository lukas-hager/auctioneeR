#' Calculate inverse bidding function
#'
#' @param mu_t a
#' @param sigma_t a
#' @param lambda_t a
#' @param k a
#' @param c a
#' @param interps a
#' @param x_seq a
#' @param n_seq a
#' @param f_weights a
#'
#' @return spline
#' @export
#'
calc_phi_individual <- function(mu_t, sigma_t, lambda_t, k, c, interps, x_seq, n_seq, f_weights){
  return(
    splinefun(
      bid_vec_individual(mu_t, sigma_t, lambda_t, k, c, interps, x_seq, n_seq, f_weights),
      mu_t + sigma_t * x_seq,
      "natural"
    )
  )
}
