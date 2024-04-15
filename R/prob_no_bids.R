prob_no_bids <- function(v, j, n, mu_t, sigma_t, lambda_t, bids, default, phi, x_s){
  a <- (j-n)*log(pnorm(x_s, mean = v, sd = sqrt(K)*sigma_t))
  e <- log(dnorm(v, mean = mu_t, sd = sigma_t))
  exp(a+e)
}
