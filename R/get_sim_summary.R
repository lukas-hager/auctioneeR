#' Summarise the simulations for an auction
#'
#' @param dt Simulated dataframe
#' @param mu_t Auction's mean
#' @param sigma_t Auction's SD
#' @param lambda_t Auction's lambda parameter
#' @param r_t Auction's reserve
#' @param k Signal standard deviation coefficient
#' @param c Cost of default
#' @param interps The interpolations for bidding
#' @param x_seq The sequence of signals
#' @param n_seq The possible number of bidders
#' @param f_weights The probability of being the high bidder
#'
#' @return A dataframe of outcomes for each possible number of bidders
#' @export
#'
get_sim_summary <- function(dt, mu_t, sigma_t, lambda_t, r_t, k, c, interps, x_seq, n_seq, f_weights){
  bids <- bid_vec_individual(
    mu_t, sigma_t, lambda_t, k, c, interps, x_seq, n_seq, f_weights
  )

  beta <- stats::splinefun(
    sigma_t*x_seq+mu_t,
    bids,
    'natural'
  )

  phi <- stats::splinefun(
    bids,
    sigma_t*x_seq+mu_t,
    'natural'
  )

  x_star <- phi(r_t)

  dt1 = dt[, .(winner = sigma_t*max(x_i)+mu_t,
               mean = sigma_t*mean(x_i)+mu_t,
               price = beta(.SD[frank(-x_i, ties.method = "first") == 2L, sigma_t*x_i+mu_t])), c('v_i', 'n')][
                 is.na(price) | price < r_t, price := r_t
               ][
                 ,
                 'trade' := winner >= x_star
               ][
                 ,
                 'default' := ((n*mean + mu_t*k)/(n+k) < price - c)*trade
               ][
                 ,
                 'revenue' := trade*(default * c + (1-default) * price)
               ][
                 ,
                 'sale_revenue' := trade * (1-default) * price
               ][
                 ,
                 'payoff' := trade*(default * -c + (1-default) * (mu_t+sigma_t*v_i-price))
               ]

  dt2 = dt1[
    ,
    .(mean_rev = mean(revenue), mean_sale_rev = mean(sale_revenue),
      mean_payoff = mean(payoff), defaults = sum(default),
      trade = sum(trade)),
    by='n'
  ][
    order(n)
  ]

  return(dt2)
}
