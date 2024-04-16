prob_no_bids <- function(v, j, n, mu_t, sigma_t, lambda_t, bids, default, phi, x_s, k){
  a <- (j-n)*log(stats::pnorm(x_s, mean = v, sd = sqrt(k)*sigma_t))
  e <- log(stats::dnorm(v, mean = mu_t, sd = sigma_t))
  exp(a+e)
}
