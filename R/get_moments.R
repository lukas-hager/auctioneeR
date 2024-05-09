#' Get the Moments of the Conditional Distribution
#'
#' @param my_signal The bidder's signal
#' @param n The number of competitors
#' @param interp_mats The matrices over which bicubic interpolation is computed
#' @param x_vals The sequence of signals for which to compute the moments
#' @param diff_vals The sequence of values to "look back" from the bidder's signal
#'
#' @return A list of matrices with the CDF and conditional expectation
#' @export
#'
get_moments <- function(my_signal, n, interp_mats, x_vals, diff_vals){
  F_ <- stats::splinefun(
    x = my_signal + diff_vals,
    y = akima::bicubic(x_vals, diff_vals, interp_mats[['f_mats']][[n-1]], rep(my_signal,length(x_vals)), diff_vals)$z,
    'natural'
  )
  E_ <- stats::splinefun(
    x = my_signal + diff_vals,
    y = akima::bicubic(x_vals, diff_vals, interp_mats[['e_mats']][[n-1]], rep(my_signal,length(x_vals)), diff_vals)$z,
    'natural'
  )
  return(list('F'=F_, 'E'=E_))
}
