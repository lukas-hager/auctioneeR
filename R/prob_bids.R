prob_bids <- function(v, j, n, mu_t, sigma_t, lambda_t, bids, default, phi, x_s){
  # dp <- default_prob(v, mu_t, sigma_t, bids, phi, signals)
  a <- (j-n)*log(pnorm(x_s, mean = v, sd = sqrt(K)*sigma_t))
  b <- log(1-pnorm(phi(bids[1]), mean = v, sd = sqrt(K)*sigma_t))
  c <-ifelse(
    rep(n>1, length(v)),
    sapply(v, function(v){sum(log(dnorm(phi(bids[-1]), mean = v, sd = sqrt(K)*sigma_t)*phi(bids[-1], deriv=1)))}),
    rep(log(1), length(v))
  )
  # d <- log(default * dp + (1-default) * (1-dp))
  e <- log(dnorm(v, mean = mu_t, sd = sigma_t))
  exp(a+b+c+e)
}
