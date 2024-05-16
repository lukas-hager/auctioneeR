#' Calculate the PDF of the Second-Order Statistic
#'
#'
#' @param y The point where we assess the distribution.
#' @param mu_0 The prior mean.
#' @param sigma_0 The prior SD.
#' @param k The coefficient on the signal standard variance.
#' @param n The number of bidders.
#'
#' @return The PDF of the second-order statistic at `y`
#' @export

f_y_y <- function(y, mu_0, sigma_0, n, k){
  sapply(y, function(y){
    riemann_sums(
      function(v){
        (n-1)*stats::dnorm(y, v, sqrt(k)*sigma_0)*stats::pnorm(y, v, sqrt(k)*sigma_0)^(n-2)*post_dist(y, mu_0, sigma_0, k)(v)/sigma_0
      },
      lower=mu_0-10*sigma_0,
      upper=mu_0+10*sigma_0,
      n_bins=100
    )
  })
}
