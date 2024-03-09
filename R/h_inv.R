#' Calculate the inverse transform of signals to posterior mean.
#'
#' Given the posterior mean is updated via \deqn{\mu_1 = h(\overline{x}) = \frac{k\mu_0 + n\overline{x}}{n+k}}
#' We have that \deqn{\overline{x}_{n-2} = h^{-1}(\mu_1) = \frac{(n+k)\mu_1 - (n-2)\overline{x}_{n-2} - 2*x_i}{n-2}}
#'
#' @param mu_1 The posterior mean.
#' @param mu_0 The prior mean.
#' @param x_i The bidder's signal.
#' @param n The number of bidders.
#' @param k The coefficient on the signal standard variance.
#'
#' @return The sample mean required to generate this value of the posterior, given a bidder's signal.
#' @export

h_inv <- function(mu_1,mu_0,x_i,n,k){
  f <- (1*(n==2) + 2*(n>2))

  return(
    ((n+k)*mu_1-k*mu_0-f*x_i)/(n-f)
  )
}
