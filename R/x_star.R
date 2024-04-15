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
# x_star <- function(r, lambda, mu_0, sigma_0, k, n_max){
#   uniroot(
#     function(x){
#       sum(
#         dpois(c(1:n_max), lambda) * sapply(c(1:n_max), function(n_val){
#           riemann_sums(
#             f=num,
#             lower=mu_0 - 10*sigma_0,
#             upper=mu_0 + 10*sigma_0,
#             x=x,
#             n=n_val,
#             mu_0=mu_0,
#             sigma_0=sigma_0,
#             k=k,
#             n_bins=1000
#           ) / riemann_sums(
#             f=den,
#             lower=mu_0 - 10*sigma_0,
#             upper=mu_0 + 10*sigma_0,
#             x=x,
#             n=n_val,
#             mu_0=mu_0,
#             sigma_0=sigma_0,
#             k=k,
#             n_bins=1000
#           )
#         })
#       ) - r
#     },
#     lower = r-5*sigma_0,
#     upper = r+10*sigma_0
#   )$root
# }
x_star <- function(r, lambda, mu_0, sigma_0, splines){
  root_val <- uniroot(
    function(x){
      sum(
        dpois(c(1:length(splines)), lambda) * sapply(c(1:length(splines)), function(n_val){
          splines[[n_val]](x)
        })
      ) / sum(dpois(c(1:length(splines)), lambda)) - (r - mu_0)/sigma_0
    },
    lower = r-100,
    upper = r+100
  )$root
  return(sigma_0 * root_val + mu_0)
}
