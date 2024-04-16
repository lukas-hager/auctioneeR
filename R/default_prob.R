default_prob <- function(v, mu_t, sigma_t, lambda_t, bids, phi, x_s, c, k, n_max){
  n_bidders_observed <- length(bids)
  second_signal <- ifelse(n_bidders_observed==1, x_s, phi(bids[2]))
  signal_sum_losers <- ifelse(n_bidders_observed==1, 0, sum(phi(bids[-1])))
  bidders <- c(n_bidders_observed:n_max)

  breakeven_v <- bids[1]-c
  l_weights <- stats::dpois(c(n_bidders_observed:n_max), lambda_t)
  inferred_v <- pmin(x_s - sqrt(k)*sigma_t*stats::qnorm(1-n_bidders_observed/bidders), 1e6)

  crit_val <- (breakeven_v - sum((l_weights/sum(l_weights)) * ((signal_sum_losers + (bidders-n_bidders_observed)*inferred_v + k*mu_t)/(k+bidders))))/(sum((1/(k+bidders))*(l_weights/sum(l_weights))))

  p_crit <- stats::pnorm((crit_val - v)/(sqrt(k)*sigma_t))
  p_loser <- stats::pnorm((second_signal - v)/(sqrt(k)*sigma_t))
  out=ifelse(
    (p_loser == 1) | (p_crit <= p_loser),
    0,
    (p_crit - p_loser)/(1-p_loser)
  )
  return(out)
}
