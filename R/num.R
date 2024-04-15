num <- function(v, x, n, mu_0, sigma_0, k){
  # f_v <- post_dist(x,mu_0,sigma_0,k,fun='pdf')
  f_v <- stats::dnorm(v, mean=mu_0, sd=sigma_0)
  f_x <- stats::dnorm(x, mean=v, sd = sqrt(k)*sigma_0)
  F_x <- stats::pnorm(x, mean=v, sd = sqrt(k)*sigma_0)
  return(v * f_v * f_x * F_x^(n-1))
}
