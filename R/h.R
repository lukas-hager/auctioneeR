#' Calculate the posterior mean from a signal and sample mean
#'
#' @param x_i The signal
#' @param x_bar The sample mean
#' @param mu_0 The prior mean
#' @param n The number of bidders
#' @param k The coefficient on the standard deviation
#'
#' @return The value of the posterior mean
#' @export
#'
h <- function(x_i, x_bar, mu_0, n, k){
  ((n-2)*x_bar + 2 * x_i + k*mu_0)/(n+k)
}
