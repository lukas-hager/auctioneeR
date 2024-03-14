#' Bid Function
#'
#' This function will calculate the optimal bid for a signal in an auction by finding the bid where the \link{eqm} function takes value zero.
#'
#'
#' @param x_i The bidder's signal.
#' @param n The number of bidders.
#' @param c The cost of default.
#' @param k The coefficient on the signal standard variance.
#' @param mu_0 The prior mean.
#' @param sigma_0 The prior standard deviation.
#'
#' @return The equilibrium bid \eqn{\beta(x_i)}
#' @export
#'

bid <- function(x_i,n,c,k,mu_0,sigma_0){
  if (n==2){
    (k*mu_0 + n*x_i)/(n+k)
  } else {
    stats::uniroot(
      eqm,
      x_i = x_i,
      n = n,
      c = c,
      k = k,
      sigma_0 = sigma_0,
      mu_0 = mu_0,
      lower = mu_0-10*sigma_0,
      upper = mu_0+10*sigma_0,
      tol=.0001,
      check.conv = TRUE
    )$root
  }
}
