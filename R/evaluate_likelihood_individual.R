evaluate_likelihood_individual <- function(MU_INT,
                                           MU_N_IMAGE,
                                           SIGMA_INT,
                                           SIGMA_N_IMAGE,
                                           K,
                                           C,
                                           L_INT,
                                           L_N_IMAGE,
                                           D_R,
                                           N_MAX){

  auction_data_iter <- auction_data %>%
    mutate(lambda_t = exp(L_INT + L_N_IMAGE*n_images),
           mu_t = MU_INT + MU_N_IMAGE*n_images,
           sigma_t = SIGMA_INT + SIGMA_N_IMAGE*n_images)

  ll <- sapply(c(1:nrow(auction_data)), function(i){
    print(i)
    # get auction-specific variables
    sigma_t <- auction_data_iter[i,'sigma_t']
    mu_t <- auction_data_iter[i,'mu_t']
    lambda_t <- auction_data_iter[i,'lambda_t']
    n <- auction_data_iter[i, 'n']
    r <- auction_data_iter[i, 'r']
    default <- auction_data_iter[i, 'default']
    bids <- auction_data_iter[i, 'bids'][[1]]

    # calculate the scaling multipliers for the signals
    a_1 <- sigma_t/SIGMA_0
    a_2 <- mu_t-MU_0*a_1

    # get the screening level
    x_s <- x_star(r, lambda_t, mu_t, sigma_t, K, N_MAX)

    # if no bids, return just probability of no bids
    if (n == 0){
      log(
        sum(
          dpois(c(0:N_MAX), lambda_t) * sapply(
            c(0:N_MAX),
            function(j){
              integrate(
                function(v){
                  a <- (j-n)*log(pnorm(x_s, mean = v, sd = K*sigma_t))+log(dnorm(v, mean = mu_t, sd = sigma_t))
                  exp(a)
                },
                lower = mu_t-20*sigma_t,
                upper = mu_t+20*sigma_t
              )$value
            }
          )
        )
      )
    } else{
      # calculate inverse bid function
      phi <- calculate_phi(
        mu_t,
        sigma_t,
        C,
        lambda_t,
        K,
        bmat
      )
      # calculate log-likelihood
      log(
        sum(
          dpois(c(n:N_MAX), lambda_t) * sapply(
            c(n:N_MAX),
            function(j){
              integrate(
                function(v){
                  crit_val <- ifelse(rep(n>1, length(v)),
                                     (n + K)*(bids[1]-C)-K*v-sum(phi(bids[-1])),
                                     rep(-Inf, length(v)))
                  prob_default <- ifelse(
                    crit_val >= bids[1] & pnorm(bids[1], mean = v, sd = K*sigma_t) < 1,
                    (pnorm(crit_val, mean = v, sd = K*sigma_t)-pnorm(bids[1], mean = v, sd = K*sigma_t)) / (1-pnorm(bids[1], mean = v, sd = K*sigma_t)),
                    0
                  )
                  a <- (j-n)*log(max(pnorm(x_s, mean = v, sd = K*sigma_t), 1e-320))
                  b <- 1-pnorm(phi(bids[1]), mean = v, sd = K*sigma_t)
                  c <-ifelse(
                    rep(n>1, length(v)),
                    sapply(v, function(v){sum(log(dnorm(phi(bids[-1]), mean = v, sd = K*sigma_t)*phi(bids[-1], deriv=1)))}),
                    rep(log(1), length(v))
                  )
                  d <- log(default * min(D_R + prob_default, 1) + (1-default) * (1-min(D_R+prob_default,1)))
                  e <- dnorm(v, mean = mu_t, sd = sigma_t)
                  exp(a+c+d)*b*e
                },
                lower = mu_t-6*sigma_t,
                upper = mu_t+6*sigma_t
              )$value
            }
          )
        )
      )
    }
  })
  # print(proc.time()-start)
  return(ll)
}
