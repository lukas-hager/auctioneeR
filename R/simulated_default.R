#' Create default decisions for simulated data
#'
#' @param signals The signals
#' @param bids The bids
#' @param xs_vals The cutoff signal
#' @param lambda The parameter governing the Poisson entry process
#' @param mu The mean of the prior distribution
#' @param sigma The standard deviation of the prior distribution
#' @param max_bidders The maximum number of bidders
#' @param c The cost of default
#' @param k The multiplier on the signals
#' @param r The reserve price
#'
#' @return Whether or not the winning bidder defaults
#' @export
#'
simulated_default <- function(signals, bids, xs_vals, lambda, mu, sigma, max_bidders,c,k,r){
  sapply(c(1:length(lambda)), function(sim_id){
    b_i <- bids[[sim_id]]
    if(length(b_i)==0){
      return(0)
    }else{
      n_bidders_observed <- length(bids[[sim_id]])
      signal_sum_observed <- sum(signals[[sim_id]][c(1:n_bidders_observed)])
      bidders <- c(n_bidders_observed:max_bidders)
      inferred_v <- pmin(xs_vals[sim_id] - sqrt(k)*sigma[sim_id]*stats::qnorm(1-n_bidders_observed/bidders), 1e6)
      lambda_weights <- stats::dpois(c(n_bidders_observed:max_bidders),lambda[sim_id])
      exp_val <- sum(lambda_weights * (signal_sum_observed + k*mu[sim_id] + (bidders-n_bidders_observed)*inferred_v) / (k+bidders))/sum(lambda_weights)
      return(as.numeric(exp_val<ifelse(length(b_i)>1, b_i[2], r)-c))
    }
  })
}
