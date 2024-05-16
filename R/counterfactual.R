maximize_counterfactual <- function(auction_data_fp, param_fp, burn_in=10000){

  auction_data_raw <- readr::read_rds(auction_data_fp)
  params_raw <- readr::read_csv(param_fp)

  params <- params_raw %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::filter(id > burn_in) %>%
    dplyr::summarise(across(!id, mean))

  auction_data_w_params <- create_auction_params(
    auction_data_raw,
    params$L_INT,
    params$L_N_IMAGE,
    params$L_YEAR,
    params$MU_INT,
    params$MU_N_IMAGE,
    params$MU_YEAR,
    params$SIGMA_INT,
    params$SIGMA_N_IMAGE,
    params$SIGMA_YEAR
  )

  set.seed(42069)
  N_MAX <- 30

  n_sims <- 100
  mu_t <- 0
  sigma_t <- 1
  k <- params$K
  # lambda_t <- 2
  # r <- -.1

  x_seq <- seq(-5, 5, length.out = 20)
  c_seq <- seq(.01, 5, length.out = 20)
  bmat <- bid_mats(k, N_MAX, x_seq, c_seq)
  # splines <- create_ev_splines(k, N_MAX)
  # xs <- x_star(r, lambda_t, mu_t, sigma_t, splines)

  n_bidders <- rep(c(1:30), n_sims)
  eps_i <- rnorm(n = sum(n_bidders), mean=0, sd = sqrt(k)*sigma_t)
  v_i <- rep(rnorm(n=length(n_bidders), mean=mu_t, sd=sigma_t), n_bidders)
  x_i <- v_i + eps_i
  n <- rep(n_bidders, n_bidders)
  dt = data.table(
    v_i,
    x_i,
    eps_i,
    n
  )

  auction_data_w_params$bpc <- sapply(c(1:nrow(auction_data_w_params)), function(i){
    calc_bid_prep_cost(
      params$C,
      auction_data_w_params$mu_t[i],
      auction_data_w_params$sigma_t[i],
      auction_data_w_params$r[i],
      auction_data_w_params$lambda_t[i]-3
    )
  })


}

#
#
# c <- params$C
# mu_t <- auction_data_w_params$mu_t[3]
# sigma_t <- auction_data_w_params$sigma_t[3]
# r_t <- auction_data_w_params$r[3]
# lambda_t <- auction_data_w_params$lambda_t[3]
# bid_prep_cost <- auction_data_w_params$bpc[2]
# #
# #
# #
# #
# #
# #
# #
# #
# calc_bid_prep_cost <- function(c, mu_t, sigma_t, r_t, lambda_t){
#   a_1 <- sigma_t
#   a_2 <- mu_t
#
#   bids <- bid_vec(
#     mu_t,
#     sigma_t,
#     x_seq,
#     c_seq,
#     c/a_1,
#     lambda_t,
#     N_MAX,
#     k,
#     bmat
#   )
#
#   beta <- stats::splinefun(
#     a_1*x_seq+a_2,
#     bids,
#     'natural'
#   )
#
#   dt1 = dt[, .(winner = sigma_t*max(x_i)+mu_t,
#                mean = sigma_t*mean(x_i)+mu_t,
#                price = beta(.SD[frank(-x_i, ties.method = "first") == 2L, sigma_t*x_i+mu_t])), c('v_i', 'n')][
#                  is.na(price) | price < r_t, price := r_t
#                ][
#                  ,
#                  'default' := (n*mean + mu_t*k)/(n+k) < price - c
#                ][
#                  ,
#                  'revenue' := default * c + (1-default) * price
#                ][
#                  ,
#                  'payoff' := default * -c + (1-default) * (mu_t+sigma_t*v_i-price)
#                ]
#
#   dt2 = dt1[
#     ,
#     .(mean_rev = mean(revenue), mean_payoff = mean(payoff), defaults = sum(default)),
#     by='n'
#   ]
#
#   sum(dpois(c(1:30), lambda_t) * dt2$mean_payoff/c(1:30))
# }
#
# counterfactual <- function(c, bid_prep_cost, mu_t, sigma_t, r_t){
#   a_1 <- sigma_t
#   a_2 <- mu_t
#
#   new_lambda <- uniroot(
#     function(new_l){
#       bids <- bid_vec(
#         mu_t,
#         sigma_t,
#         x_seq,
#         c_seq,
#         c/a_1,
#         new_l,
#         N_MAX,
#         k,
#         bmat
#       )
#
#       beta <- stats::splinefun(
#         a_1*x_seq+a_2,
#         bids,
#         'natural'
#       )
#
#       dt1 = dt[, .(winner = sqrt(k)*sigma_t*max(x_i)+mu_t,
#                    mean = sqrt(k)*sigma_t*mean(x_i)+mu_t,
#                    price = beta(.SD[frank(-x_i, ties.method = "first") == 2L, sqrt(k)*sigma_t*x_i+mu_t])), c('v_i', 'n')][
#                      is.na(price) | price < r_t, price := r_t
#                    ][
#                      ,
#                      'default' := (n*mean + mu_t*k)/(n+k) < price - c
#                    ][
#                      ,
#                      'revenue' := default * c + (1-default) * price
#                    ][
#                      ,
#                      'payoff' := default * -c + (1-default) * (mu_t+sigma_t*v_i-price)
#                    ]
#
#       dt2 = dt1[
#         ,
#         .(mean_rev = mean(revenue), mean_payoff = mean(payoff)),
#         by='n'
#       ]
#
#       sum(dpois(c(1:30), new_l) * dt2$mean_payoff/c(1:30)) - bid_prep_cost
#     },
#     lower=.0001,
#     upper=10
#   )$root
#
#   bids <- bid_vec(
#     mu_t,
#     sigma_t,
#     x_seq,
#     c_seq,
#     c/a_1,
#     new_lambda,
#     N_MAX,
#     k,
#     bmat
#   )
#
#   beta <- stats::splinefun(
#     a_1*x_seq+a_2,
#     bids,
#     'natural'
#   )
#
#   dt1 = dt[, .(winner = sqrt(k)*sigma_t*max(x_i)+mu_t,
#                mean = sqrt(k)*sigma_t*mean(x_i)+mu_t,
#                price = beta(.SD[frank(-x_i, ties.method = "first") == 2L, sqrt(k)*sigma_t*x_i+mu_t])), c('v_i', 'n')][
#                  is.na(price), price := r_t
#                ][
#                  ,
#                  'default' := (mean + mu_t*k)/(n+k) < price - c
#                ][
#                  ,
#                  'revenue' := default * c + (1-default) * price
#                ][
#                  ,
#                  'payoff' := default * -c + (1-default) * (mu_t+sqrt(k)*sigma_t*v_i-price)
#                ]
#
#   dt2 = dt1[
#     ,
#     .(mean_rev = mean(revenue), mean_payoff = mean(payoff)),
#     by='n'
#   ]
#
#   sum(dpois(c(1:30), new_lambda) * dt2$mean_rev)
# }
