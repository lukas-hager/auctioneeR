#' Equilibrium Function
#'
#' This function will take value zero for the equilibrium bid `b`.
#'
#'
#' @param b The proposed bid.
#' @param n The number of bidders.
#' @param c The cost of default.
#' @param k The coefficient on the signal standard variance.
#' @param my_signal The bidder's signal
#' @param interps The matrices used for interpolation
#'
#' @return The error of the equilibrium function at bid `b`.
#' @export
#'

eqm <- function(b, my_signal, n, c, k, interps){
  threshold_x_bar <- h_inv(b-c,0,my_signal,n,k)
  p_default <- threshold_x_bar > my_signal + (threshold_x_bar <= my_signal) * pmin(pmax(interps[['F']](threshold_x_bar),0), 1)
  p_default*c + (1-p_default)*b - (1-p_default) * h(my_signal, interps[['E']](threshold_x_bar), 0, n, k)
}
