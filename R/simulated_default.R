simulated_default <- function(signals, bids, xs_vals, lambda, mu, sigma, max_bidders,c,k,r){
  sapply(c(1:length(lambda)), function(sim_id){
    b_i <- bids[[sim_id]]
    if(length(b_i)==0){
      return(0)
    }else{
      n_bidders_observed <- length(bids[[sim_id]])
      signal_sum_observed <- sum(signals[[sim_id]][c(1:n_bidders_observed)])
      bidders <- c(n_bidders_observed:max_bidders)
      # exp_given_n <- (signal_sum_observed + k*mu[sim_id] + (c(n_bidders_observed:max_bidders)-n_bidders_observed)*xs_vals[[sim_id]]) / (k + c(n_bidders_observed:max_bidders))
      # exp_val <- sum(
      #   (dpois(c(n_bidders_observed:max_bidders), lambda[[sim_id]]) * exp_given_n) / sum(dpois(c(n_bidders_observed:max_bidders), lambda[[sim_id]]))
      # )

      # v_exp <- sapply(c(n_bidders_observed:max_bidders),function(n_bidders){
      #   uniroot(
      #     function(v){
      #       signal_sum_observed + k*mu[sim_id] + (n_bidders-n_bidders_observed)*(v - sqrt(k*sigma[sim_id]^2)*stats::dnorm((xs_vals[sim_id]-v)/sqrt(k*sigma[sim_id]^2))/stats::pnorm((xs_vals[sim_id]-v)/sqrt(k*sigma[sim_id]^2))) - v*(1+n_bidders)
      #     },
      #     lower = mu[sim_id]-5*sigma[sim_id],
      #     upper = mu[sim_id]+5*sigma[sim_id]
      #   )$root
      # })
      inferred_v <- pmin(xs_vals[sim_id] - sqrt(k)*sigma[sim_id]*stats::qnorm(1-n_bidders_observed/bidders), 1e6)


      lambda_weights <- stats::dpois(c(n_bidders_observed:max_bidders),lambda[sim_id])

      exp_val <- sum(lambda_weights * (signal_sum_observed + k*mu[sim_id] + (bidders-n_bidders_observed)*inferred_v) / (k+bidders))/sum(lambda_weights)


      # signal_sum <- sum(signals[[sim_id]])
      # n_bidders <- length(signals[[sim_id]])
      # exp_val <- (signal_sum + k*mu[[sim_id]]) / (n_bidders+k)
      return(as.numeric(exp_val<ifelse(length(b_i)>1, b_i[2], r)-c))
    }
  })
}


# simulated_default <- function(signals, bids, xs_vals, lambda, mu, sigma, max_bidders,c,k,r){
#   sapply(c(1:length(lambda)), function(sim_id){
#     b_i <- bids[[sim_id]]
#     if(length(b_i)==0){
#       return(0)
#     }else{
#       x_i <- sort(signals[[sim_id]], decreasing = TRUE)
#       x_s <- xs_vals[sim_id]
#       lambda_t <- lambda[sim_id]
#       mu_t <- mu[sim_id]
#       sigma_t <- sigma[sim_id]
#       possible_bidders <- c(length(b_i)):max_bidders
#
#       n_bidder_prob <- dpois(possible_bidders, lambda_t)
#       signal_sum <- sum(x_i[1:length(b_i)])
#       beta <- (x_s-mu_t)/(k*sigma_t)
#       non_observed_mean <- mu_t - k*sigma_t*stats::dnorm(beta)/stats::pnorm(beta)
#       non_observed_bidders <- c(0:(max_bidders-length(b_i)))
#       exp_given_n <- (signal_sum + non_observed_mean*non_observed_bidders + k*mu_t)/(k+length(b_i) + non_observed_bidders)
#       exp_val <- sum(n_bidder_prob * exp_given_n) / sum(n_bidder_prob)
#       return(as.numeric(exp_val<ifelse(length(b_i)>1, b_i[2], r)-c))
#     }
#   })
# }
