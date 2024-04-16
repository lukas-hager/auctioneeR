prob_default_given_bids <- function(v, mu_t, sigma_t, lambda_t, bids, phi, default, x_s, c, k, n_max){
  dp <- default_prob(v, mu_t, sigma_t, lambda_t, bids, phi, x_s, c, k, n_max)
  d <- log(max(default * dp + (1-default) * (1-dp),0))
  e <- log(stats::dnorm(v, mean = mu_t, sd = sigma_t))
  exp(d+e)
}
