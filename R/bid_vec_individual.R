#' Calculate Individual Bid Vector
#'
#' @param mu_t a
#' @param sigma_t a
#' @param lambda_t a
#' @param k a
#' @param c a
#' @param interps a
#' @param x_seq a
#' @param n_seq a
#' @param f_weights a
#'
#' @return Bid Vector
#' @export
#'
bid_vec_individual <- function(mu_t, sigma_t, lambda_t, k, c, interps, x_seq, n_seq, f_weights){
  p_weights <- dpois(n_seq, lambda_t)

  bids <- sapply(mu_t + sigma_t * x_seq, function(x){
    # print(x)

    uniroot(
      function(b){
        idx <- which(mu_t + sigma_t * x_seq == x)
        i <- interps[[idx]]
        threshold_x_bar <- c(mu_t-10*sigma_t, h_inv(b-c,mu_t,x,n_seq[-1],k))
        p_default <- c(0,as.numeric(threshold_x_bar[-1] > x + (threshold_x_bar[-1] <= x) * pmin(pmax(sapply(n_seq[-1], function(n){i[[n-1]][['F']]((threshold_x_bar[n-1]-mu_t)/sigma_t)}),0), 1)))
        exp <- sapply(n_seq, function(n){
          h(x, mu_t + sigma_t*i[[n-1]][['E']]((threshold_x_bar[n-1]-mu_t)/sigma_t), mu_t, n, k)
        })
        sum(f_weights[,idx]*p_weights*(p_default*c+(1-p_default)*b)) - sum(p_weights*f_weights[,idx]*(1-p_default)*exp)
      },
      lower = mu_t - 15*sigma_t,
      upper = mu_t + 15*sigma_t,
      check.conv = TRUE
    )$root
  })
}
#
# start <- proc.time()
# bid_vec_individual(mu_t, sigma_t, lambda_t, k, c, interps)
# proc.time()-start
#
# ggplot() + geom_point(aes(x=mu_t + sigma_t * x_seq, y = bids)) + geom_abline()
#
# beta <- splinefun(
#   mu_t + sigma_t * x_seq,
#   bids,
#   'natural'
# )
#
# set.seed(42069)
# N_MAX <- 30
#
# n_sims <- 100
# mu_t <- 0
# sigma_t <- 1
#
# n_bidders <- rep(c(1:30), n_sims)
# eps_i <- rnorm(n = sum(n_bidders), mean=0, sd = sqrt(k)*sigma_t)
# v_i <- rep(rnorm(n=length(n_bidders), mean=mu_t, sd=sigma_t), n_bidders)
# x_i <- v_i + eps_i
# n <- rep(n_bidders, n_bidders)
# dt = data.table(
#   v_i,
#   x_i,
#   eps_i,
#   n
# )
#
# dt1 = dt[, .(winner = sigma_t*max(x_i)+mu_t,
#              mean = sigma_t*mean(x_i)+mu_t,
#              price = beta(.SD[frank(-x_i, ties.method = "first") == 2L, sigma_t*x_i+mu_t])), c('v_i', 'n')][
#                is.na(price) | price < r_t, price := r_t
#              ][
#                ,
#                'default' := (n*mean + mu_t*k)/(n+k) < price - c
#              ][
#                ,
#                'revenue' := default * c + (1-default) * price
#              ][
#                ,
#                'payoff' := default * -c + (1-default) * (mu_t+sigma_t*v_i-price)
#              ]
#
# dt2 = dt1[
#   ,
#   .(mean_rev = mean(revenue), mean_payoff = mean(payoff), defaults = sum(default)),
#   by='n'
# ]
#
# sum(dpois(c(1:30), lambda_t) * dt2$mean_payoff/c(1:30))
