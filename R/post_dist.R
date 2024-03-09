#' Posterior Distribution of Item's Value
#'
#' @param x The observed data.
#' @param mu_0 The prior mean.
#' @param sigma_0 The prior standard deviation.
#' @param k The coefficient on the signal standard deviation
#'
#' @return A `stats::dnorm` object with the appropriate hyperparameters.
#' @export
#'
post_dist <- function(x, mu_0, sigma_0, k){
  n <- length(x)
  return(
    function(val){
      stats::dnorm(
        val,
        mean = (k*mu_0 + sum(x))/(n+k),
        sd = sqrt((1/sigma_0^2) + (n/(k*sigma_0^2)))
      )
    }
  )
}
