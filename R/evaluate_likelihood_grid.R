#' Calculate the likelihood of the data for a set of parameters
#'
#' @param auction_data The dataset to calculate likelihood over
#' @param MU_INT The intercept of the \eqn{\mu} parameter
#' @param MU_N_IMAGE The coefficient on `n_images` of the \eqn{\mu} parameter
#' @param SIGMA_INT The intercept of the \eqn{\sigma} parameter
#' @param SIGMA_N_IMAGE The coefficient on `n_images` of the \eqn{\sigma} parameter
#' @param MU_YEAR The coefficient on `year`
#' @param SIGMA_YEAR The coefficient on `year`
#' @param L_YEAR The coefficient on `year`
#' @param K The multiplier on
#' @param C a
#' @param L_INT a
#' @param L_N_IMAGE a
#' @param D_R a
#' @param N_MAX a
#' @param par a
#'
#' @return The log-likelihood of observing the data
#' @export

evaluate_likelihood_grid  <- function(auction_data,
                                      MU_INT,
                                      MU_N_IMAGE,
                                      MU_YEAR,
                                      SIGMA_INT,
                                      SIGMA_N_IMAGE,
                                      SIGMA_YEAR,
                                      L_INT,
                                      L_N_IMAGE,
                                      L_YEAR,
                                      K,
                                      C,
                                      D_R,
                                      N_MAX,
                                      par=FALSE){
  start <- proc.time()

  x_seq <- seq(-5, 5, length.out = 20)
  c_seq <- seq(.01, 5, length.out = 20)
  bmat <- bid_mats(K, N_MAX, x_seq, c_seq)
  # splines <- create_ev_splines(K, N_MAX)

  auction_data_iter <- create_auction_params(
    auction_data,L_INT,L_N_IMAGE,L_YEAR,MU_INT,
    MU_N_IMAGE,MU_YEAR,SIGMA_INT,SIGMA_N_IMAGE,SIGMA_YEAR
  )

  if(!par){
    ll <- sapply(c(1:nrow(auction_data)), function(i){
      # get auction-specific variables
      sigma_t <- auction_data_iter[i,'sigma_t']
      mu_t <- auction_data_iter[i,'mu_t']
      lambda_t <- auction_data_iter[i,'lambda_t']
      n <- auction_data_iter[i, 'n']
      r <- auction_data_iter[i, 'r']
      default <- auction_data_iter[i, 'default']
      bids <- auction_data_iter[i, 'bids'][[1]]

      # calculate the scaling multipliers for the signals
      a_1 <- sigma_t
      a_2 <- mu_t

      # get the screening level
      x_s <- x_star(r, lambda_t, mu_t, sigma_t, splines)

      # if no bids, return just probability of no bids
      if (n == 0){
        log(
          sum(
            stats::dpois(c(1:N_MAX), lambda_t) * sapply(
              c(1:N_MAX),
              function(j){
                riemann_sums(
                  f=prob_no_bids,
                  j=j,
                  n=n,
                  mu_t=mu_t,
                  sigma_t=sigma_t,
                  lambda_t=lambda_t,
                  bids=bids,
                  default=default,
                  x_s=x_s,
                  k=K,
                  lower=mu_t-5*sigma_t,
                  upper=mu_t+5*sigma_t,
                  n_bins = 100
                )
              }
            )
          ) + stats::dpois(0, lambda_t)
        )
      } else{
        # calculate inverse bid function
        phi <- calc_phi_grid(
          mu_t,
          sigma_t,
          mu_0=0,
          sigma_0=1,
          x_seq,
          c_seq,
          C / a_1,
          lambda_t,
          N_MAX,
          K,
          bmat
        )
        # calculate log-likelihood
        log(
          sum(
            stats::dpois(c(n:N_MAX), lambda_t) * sapply(
              c(n:N_MAX),
              function(j){
                riemann_sums(
                  f=prob_bids,
                  j=j,
                  n=n,
                  mu_t=mu_t,
                  sigma_t=sigma_t,
                  lambda_t=lambda_t,
                  bids=bids,
                  default=default,
                  x_s=x_s,
                  phi=phi,
                  k=K,
                  lower=mu_t-5*sigma_t,
                  upper=mu_t+5*sigma_t,
                  n_bins = 100
                )
              }
            )
          )
        ) + log(
          riemann_sums(
            f=prob_default_given_bids,
            lower=mu_t-5*sigma_t,
            upper=mu_t+5*sigma_t,
            n_bins=100,
            mu_t=mu_t,
            sigma_t=sigma_t,
            bids=bids,
            phi=phi,
            default=default,
            c=C,
            k=K,
            n_max=N_MAX
          )
        )
      }
    })
  }else{
    ll <- foreach::foreach(
      i = c(1:nrow(auction_data)),
      .combine = 'c'
    ) %dopar% {
      # get auction-specific variables
      sigma_t <- auction_data_iter[i,'sigma_t']
      mu_t <- auction_data_iter[i,'mu_t']
      lambda_t <- auction_data_iter[i,'lambda_t']
      n <- auction_data_iter[i, 'n']
      r <- auction_data_iter[i, 'r']
      default <- auction_data_iter[i, 'default']
      bids <- auction_data_iter[i, 'bids'][[1]]

      # calculate the scaling multipliers for the signals
      a_1 <- sigma_t
      a_2 <- mu_t

      # calculate inverse bid function
      phi <- calc_phi_grid(
        mu_t,
        sigma_t,
        mu_0=0,
        sigma_0=1,
        x_seq,
        c_seq,
        C / a_1,
        lambda_t,
        N_MAX,
        K,
        bmat
      )

      # get the screening level
      # x_s <- x_star(r, lambda_t, mu_t, sigma_t, splines)

      x_s <- phi(r)

      # if no bids, return just probability of no bids
      if (n == 0){
        log(
          sum(
            stats::dpois(c(1:N_MAX), lambda_t) * sapply(
              c(1:N_MAX),
              function(j){
                riemann_sums(
                  f=prob_no_bids,
                  j=j,
                  n=n,
                  mu_t=mu_t,
                  sigma_t=sigma_t,
                  lambda_t=lambda_t,
                  bids=bids,
                  default=default,
                  x_s=x_s,
                  k=K,
                  lower=mu_t-20*sigma_t,
                  upper=mu_t+20*sigma_t,
                  n_bins = 100
                )
              }
            )
          ) + stats::dpois(0, lambda_t)
        )
      } else{
        # calculate log-likelihood
        log(
          sum(
            stats::dpois(c(n:N_MAX), lambda_t) * sapply(
              c(n:N_MAX),
              function(j){
                riemann_sums(
                  f=prob_bids,
                  j=j,
                  n=n,
                  mu_t=mu_t,
                  sigma_t=sigma_t,
                  lambda_t=lambda_t,
                  bids=bids,
                  default=default,
                  x_s=x_s,
                  phi=phi,
                  k=K,
                  lower=mu_t-20*sigma_t,
                  upper=mu_t+20*sigma_t,
                  n_bins = 100
                )
              }
            )
          )
        ) + log(
          riemann_sums(
            f=prob_default_given_bids,
            lower=mu_t-20*sigma_t,
            upper=mu_t+20*sigma_t,
            n_bins=100,
            mu_t=mu_t,
            sigma_t=sigma_t,
            lambda_t=lambda_t,
            bids=bids,
            phi=phi,
            default=default,
            x_s=x_s,
            c=C,
            k=K,
            n_max=N_MAX,
            dr=1e-7
          )
        )
      }
    }
  }
  # print(ll)
  print(sum(ll))
  return(sum(ll))
}
