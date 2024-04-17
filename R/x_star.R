#' Calculate the Threshold Signal Value
#'
#' @param r The auction's reserve price.
#' @param lambda The poisson parameter dictating entry.
#' @param mu_0 The prior distribution's mean.
#' @param sigma_0 The prior distribution's standard deviation.
#' @param splines The base case splines.
#'
#' @return The threshold signal
#' @export
#'
x_star <- function(r, lambda, mu_0, sigma_0, splines){
  root_val <- stats::uniroot(
    function(x){
      sum(
        stats::dpois(c(1:length(splines)), lambda) * sapply(c(1:length(splines)), function(n_val){
          splines[[n_val]](x)
        })
      ) / sum(stats::dpois(c(1:length(splines)), lambda)) - (r - mu_0)/sigma_0
    },
    lower = r-1000,
    upper = r+1000
  )$root
  return(sigma_0 * root_val + mu_0)
}
