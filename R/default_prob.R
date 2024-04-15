# default_prob <- function(v, j, n, mu_t, sigma_t, lambda_t, bids, default, phi, x_s){
#   possible_bidders <- c(length(bids):N_MAX)
#   n_bidder_prob <- dpois(possible_bidders, lambda_t)
#   signal_sum_losers <- sum(phi(bids[-1]))
#   beta <- (x_s-v)/(sqrt(K)*sigma_t)
#   non_observed_mean <- v - sqrt(K)*sigma_t*stats::dnorm(beta)/stats::pnorm(beta)
#   non_observed_bidders <- c(0:(N_MAX-length(bids)))
#
#   num <- sum(n_bidder_prob) * (bids[1]-C) - sum(n_bidder_prob * (signal_sum_losers + non_observed_bidders * non_observed_mean) / (K + possible_bidders))
#   den <- sum(n_bidder_prob / (K + possible_bidders))
#   crit_val <- (num - sum(n_bidder_prob / (K+possible_bidders)) * (K+v)) / den
#
#   return(stats::pnorm((crit_val - v)/(sqrt(K)*sigma_t)))
# }


default_prob <- function(v, mu_t, sigma_t, lambda_t, bids, phi, x_s, c){
  n_bidders_observed <- length(bids)
  second_signal <- ifelse(n_bidders_observed==1, x_s, phi(bids[2]))
  signal_sum_losers <- ifelse(n_bidders_observed==1, 0, sum(phi(bids[-1])))
  bidders <- c(n_bidders_observed:N_MAX)

  breakeven_v <- bids[1]-c
  l_weights <- dpois(c(n_bidders_observed:N_MAX), lambda_t)
  inferred_v <- pmin(x_s - sqrt(K)*sigma_t*qnorm(1-n_bidders_observed/bidders), 1e6)

  crit_val <- (breakeven_v - sum((l_weights/sum(l_weights)) * ((signal_sum_losers + (bidders-n_bidders_observed)*inferred_v + K*mu_t)/(K+bidders))))/(sum((1/(K+bidders))*(l_weights/sum(l_weights))))

  p_crit <- stats::pnorm((crit_val - v)/(sqrt(K)*sigma_t))
  p_loser <- stats::pnorm((second_signal - v)/(sqrt(K)*sigma_t))
  out=ifelse(
    (p_loser == 1) | (p_crit <= p_loser),
    0,
    (p_crit - p_loser)/(1-p_loser)
  )
  # probs <- sapply(v, function(v_val){
  #   breakeven_beta <- (x_s-v_val)/(sqrt(K)*sigma_t)
  #   crit_val <- uniroot(
  #     function(x){
  #       exps <- sapply(c(n_bidders_observed:N_MAX),function(n_bidders){
  #         uniroot(
  #           function(v_){
  #             x + signal_sum_losers + K*mu_t + (n_bidders-n_bidders_observed)*(v_ - sqrt(K)*sigma_t*stats::dnorm(breakeven_beta)/stats::pnorm(breakeven_beta)) - v_*(K+n_bidders)
  #           },
  #           lower = mu_t-1000*sigma_t,
  #           upper = mu_t+1000*sigma_t
  #         )$root
  #       })
  #       sum(l_weights * exps) / sum(l_weights) - breakeven_v
  #     },
  #     lower = mu_t-1000*sigma_t,
  #     upper = mu_t+1000*sigma_t
  #   )$root
    # p_crit <- stats::pnorm((crit_val - v_val)/(sqrt(K)*sigma_t))
    # p_loser <- stats::pnorm((second_signal - v_val)/(sqrt(K)*sigma_t))
    # out=ifelse(
    #   (p_loser == 1) | (p_crit <= p_loser),
    #   0,
    #   (p_crit - p_loser)/(1-p_loser)
    # )
  # })
  return(out)
  # probs <- sapply(v, function(v_val){
  #   breakeven_beta <- (x_s-v_val)/(sqrt(K)*sigma_t)
  #   breakeven_x <- breakeven_v*(1+bidders) - signal_sum_losers - K*mu_t - (
  #     bidders-n_bidders_observed
  #   )*(
  #     breakeven_v - sqrt(K)*sigma_t*stats::dnorm(breakeven_beta)/stats::pnorm(breakeven_beta)
  #   )
  #   crit_signals <- (K + bidders)*(bids[1]-c)-(signal_sum_losers + K*mu_t + (c(n_bidders_observed:N_MAX)-n_bidders_observed)*x_s)
  #   p_crit <- stats::pnorm((crit_signals - v_val)/(sqrt(K)*sigma_t))
  #   p_loser <- stats::pnorm((second_signal - v_val)/(sqrt(K)*sigma_t))
  #   out=ifelse(
  #     (p_loser == 1) | (p_crit <= p_loser),
  #     0,
  #     (p_crit - p_loser)/(1-p_loser)
  #   )
  #   total_prob <- sum(
  #     (out * l_weights / sum(l_weights))
  #     )
  # })
  #
  # return(probs)
}



# beta <- (x_s-v)/(sqrt(K)*sigma_t)
# non_observed_mean <- v - sqrt(K)*sigma_t*stats::dnorm(beta)/stats::pnorm(beta)
# non_observed_bidders <- j-n

# # crit_val <- (bids[1]-C)*(K+j)-signal_sum_losers-non_observed_bidders*non_observed_mean-K*v
# crit_val <- (bids[1]-c)*(K+length(signals))-signal_sum_losers-K*mu_t
# p_crit <- stats::pnorm((crit_val - v)/(sqrt(K)*sigma_t))
# p_loser <- stats::pnorm((second_signal - v)/(sqrt(K)*sigma_t))
# # print(p_crit - p_loser)
#
# out=ifelse(
#   (p_loser == 1) | (crit_val <= second_signal),
#   0,
#   (p_crit - p_loser)/(1-p_loser)
# )
#
# return(out)
