# maximize_counterfactual <- function(auction_data_fp, param_fp, burn_in=10000, n_cores =6){
#
#   auction_data_raw <- readr::read_rds(auction_data_fp)
#   params_raw <- readr::read_csv(param_fp)
#
#   params <- params_raw %>%
#     dplyr::mutate(id = dplyr::row_number()) %>%
#     dplyr::filter(id > burn_in) %>%
#     dplyr::summarise(across(!id, mean))
#
#   auction_data_w_params <- create_auction_params(
#     auction_data_raw,
#     params$L_INT,
#     params$L_N_IMAGE,
#     params$L_YEAR,
#     params$MU_INT,
#     params$MU_N_IMAGE,
#     params$MU_YEAR,
#     params$SIGMA_INT,
#     params$SIGMA_N_IMAGE,
#     params$SIGMA_YEAR
#   )
#
#   my_cluster <- parallel::makeCluster(
#     n_cores,
#     type = "FORK"
#   )
#
#   doParallel::registerDoParallel(cl = my_cluster)
#
#   # auction_data_w_params$bpc <- sapply(c(1:nrow(auction_data_w_params)), function(i){
#   #   calc_bid_prep_cost(
#   #     params$C,
#   #     auction_data_w_params$mu_t[i],
#   #     auction_data_w_params$sigma_t[i],
#   #     auction_data_w_params$r[i],
#   #     auction_data_w_params$lambda_t[i]
#   #   )
#   # })
#
#   auction_data_w_params$bpc <- foreach::foreach(
#     i = c(1:nrow(auction_data_w_params)),
#     .combine='c'
#   ) %dopar% {
#     calc_bid_prep_cost(
#       params$C,
#       auction_data_w_params$mu_t[i],
#       auction_data_w_params$sigma_t[i],
#       auction_data_w_params$r[i],
#       auction_data_w_params$lambda_t[i]
#     )
#   }
#
#   parallel::stopCluster(my_cluster)
# }
#
# #
# #
# x_seq <- seq(-5, 5, length.out = 20)
# diff_vals <- seq(-4, 0, length.out=20)
#
# # create interpolation grids for beliefs about default
# interp_grids <- create_interp_mats(k, 30, x_seq, diff_vals)
#
# # get the values of the interpolations at the points we assess
#
# n_seq <- c(2:30)
# interps <- lapply(x_seq, function(x_val){
#   lapply(n_seq, function(n_val){
#     get_moments(x_val, n_val, interp_grids, x_seq, diff_vals)
#   })
# })
#
# f_weights <- rbind(
#   sapply(x_seq, function(x_val){
#     sapply(n_seq, function(n){f_y_y(x_val, 0, 1, n, k)})
#   })
# )
#
# c <- params$C
# out <- sapply(c(1:nrow(auction_data_w_params)), function(i){
#   mu_t <- auction_data_w_params$mu_t[i]
#   sigma_t <- auction_data_w_params$sigma_t[i]
#   r_t <- auction_data_w_params$r[i]
#   lambda_t <- auction_data_w_params$lambda_t[i]
#
#
#   bids <- bid_vec_individual(
#     mu_t, sigma_t, lambda_t, k, c, interps, x_seq, n_seq, f_weights
#   )
#
#   beta <- stats::splinefun(
#     sigma_t*x_seq+mu_t,
#     bids,
#     'natural'
#   )
#
#   phi <- stats::splinefun(
#     bids,
#     sigma_t*x_seq+mu_t,
#     'natural'
#   )
#
#   x_star <- phi(r_t)
#
#   dt1 = dt[, .(winner = sigma_t*max(x_i)+mu_t,
#                mean = sigma_t*mean(x_i)+mu_t,
#                price = beta(.SD[frank(-x_i, ties.method = "first") == 2L, sigma_t*x_i+mu_t])), c('v_i', 'n')][
#                  is.na(price) | price < r_t, price := r_t
#                ][
#                  ,
#                  'trade' := winner >= x_star
#                ][
#                  ,
#                  'default' := (n*mean + mu_t*k)/(n+k) < price - c
#                ][
#                  ,
#                  'revenue' := trade*(default * c + (1-default) * price)
#                ][
#                  ,
#                  'payoff' := trade*(default * -c + (1-default) * (mu_t+sigma_t*v_i-price))
#                ]
#
#   dt2 = dt1[
#     ,
#     .(mean_rev = mean(revenue), mean_payoff = mean(payoff), defaults = sum(default), trade = sum(trade)),
#     by='n'
#   ][
#     order(n)
#   ]
#
#   sum(dpois(c(1:30), lambda_t) * dt2$mean_payoff/c(1:30))
#
# })
#
#
#
#
#
#
#
#
#
#
#
# x_seq <- seq(-5, 5, length.out = 20)
# diff_vals <- seq(-4, 0, length.out=20)
#
# # create interpolation grids for beliefs about default
# interp_grids <- create_interp_mats(k, 30, x_seq, diff_vals)
#
# # get the values of the interpolations at the points we assess
#
# n_seq <- c(2:30)
# interps <- lapply(x_seq, function(x_val){
#   lapply(n_seq, function(n_val){
#     get_moments(x_val, n_val, interp_grids, x_seq, diff_vals)
#   })
# })
#
# f_weights <- rbind(
#   sapply(x_seq, function(x_val){
#     sapply(n_seq, function(n){f_y_y(x_val, 0, 1, n, k)})
#   })
# )
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
# uniroot(
#   function(l){
#   calc_bid_prep_cost(dt, auction_data_w_params$mu_t[i],
#                      auction_data_w_params$sigma_t[i],
#                      l,
#                      auction_data_w_params$r[i],
#                      k,
#                      100,
#                      interps,
#                      x_seq,
#                      n_seq,
#                      f_weights) - auction_data_w_params$bpc[i]},
#   lower=1,
#   upper=100
# )
#
# summ <- get_sim_summary(
#   dt, auction_data_w_params$mu_t[i], auction_data_w_params$sigma_t[i], 20.53,
#   auction_data_w_params$r[i], k, 100, interps, x_seq, n_seq, f_weights
# )
# sum(dpois(c(1:30), 20.53) * summ$mean_rev)
#
# summ <- get_sim_summary(
#   dt, auction_data_w_params$mu_t[i], auction_data_w_params$sigma_t[i], auction_data_w_params$lambda_t[i],
#   auction_data_w_params$r[i], k, c, interps, x_seq, n_seq, f_weights
# )
# sum(dpois(c(1:30), auction_data_w_params$lambda_t[i]) * summ$mean_rev)
#
# counterfactual(
#   dt, 100,
#   auction_data_w_params$bpc[i],
#   auction_data_w_params$mu_t[i],
#   auction_data_w_params$sigma_t[i],
#   auction_data_w_params$lambda_t[i],
#   auction_data_w_params$r[i],
#   params$K,
#   interps,
#   x_seq,
#   n_seq,
#   f_weights
# )
#
#
# my_cluster <- parallel::makeCluster(
#   n_cores,
#   type = "FORK"
# )
#
# doParallel::registerDoParallel(cl = my_cluster)
#
# out <- sapply(seq(10, 120, by =10), function(c_val){
#
#   cf_vals <- foreach::foreach(
#     i = c(1:500),
#     .combine='c'
#   ) %dopar% {
#     counterfactual(
#       c_val,dt,
#       auction_data_w_params$bpc[i],
#       auction_data_w_params$mu_t[i],
#       auction_data_w_params$sigma_t[i],
#       auction_data_w_params$lambda_t[i],
#       auction_data_w_params$r[i],
#       params$K,
#       interps,
#       x_seq,
#       n_seq,
#       f_weights
#     )
#   }
#   print(c(c_val, sum(cf_vals)))
#   sum(cf_vals)
# })
#
# out_resale <- sapply(seq(10, 120, by =10), function(c_val){
#
#   cf_vals <- foreach::foreach(
#     i = c(1:10),
#     .combine='c'
#   ) %dopar% {
#     counterfactual_resale(
#       c_val,dt,
#       auction_data_w_params$bpc[i],
#       auction_data_w_params$mu_t[i],
#       auction_data_w_params$sigma_t[i],
#       auction_data_w_params$lambda_t[i],
#       auction_data_w_params$r[i],
#       params$K,
#       interps,
#       x_seq,
#       n_seq,
#       f_weights
#     )
#   }
#   print(c(c_val, sum(cf_vals)))
#   sum(cf_vals)
# })
#
# parallel::stopCluster(my_cluster)
#
#
# optim(
#   100,
#   counterfactual,
#   dt,
#   auction_data_w_params$bpc[i],
#   auction_data_w_params$mu_t[i],
#   auction_data_w_params$sigma_t[i],
#   auction_data_w_params$lambda_t[i],
#   auction_data_w_params$r[i],
#   params$K,
#   interps,
#   x_seq,
#   n_seq,
#   f_weights
# )
