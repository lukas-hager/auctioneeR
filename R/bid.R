#' Bid Function
#'
#' This function will calculate the optimal bid for a signal in an auction by finding the bid where the \link{eqm} function takes value zero.
#'
#'
#' @param x_i The bidder's signal.
#' @param n The number of bidders.
#' @param c The cost of default.
#' @param interps The matrices used for interpolation
#' @param k The coefficient on the signal standard variance.
#'
#' @return The equilibrium bid \eqn{\beta(x_i)}
#' @export
#'

bid <- function(x_i,n,c,k,interps){
  if (n==2){
    h(x_i, interps[['E']](-10), 0, n, k)
  } else {
    stats::uniroot(
      eqm,
      my_signal=x_i,
      n=n,
      c=c,
      k=k,
      interps=interps,
      lower = -20,
      upper = 20,
      tol=.0001,
      check.conv = TRUE
    )$root
  }
}
