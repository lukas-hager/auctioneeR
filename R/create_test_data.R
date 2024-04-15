#' Generate test data for algorithm
#'
#' @param n_sims The number of observations to generate
#' @param seed The random seed. Defaults to 42069.
#'
#' @return A dataframe of simulated auction data
#' @export
#'
create_test_data <- function(n_sims, seed=42069){
  set.seed(seed)

  n_images <- round(runif(n_sims, 2, 10))
  lambda <- exp(.1 + .1*n_images)
  mu <- 50 + 2*n_images
  sigma <- 10 + .5*n_images
  k <- .5
  c <- 10
  r <- 25
  d_r <- .03
  n_bidders <- rpois(n_sims, lambda)
  n_max <- 30
  v <- rnorm(n=n_sims, mean=mu, sd=sigma)
  signals <- sapply(c(1:n_sims), function(sim_id){
    sort(rnorm(
      n_bidders[sim_id],
      mean = v[sim_id],
      sd = sqrt(k)*sigma[sim_id]
    ), decreasing = TRUE)
  })
  splines <- create_ev_splines(k, n_max)
  xs_vals <- sapply(c(1:n_sims), function(sim_id){
    x_star(r, lambda[sim_id], mu[sim_id], sigma[sim_id], splines)
  })
  bids <- sapply(c(1:n_sims),function(sim_id){
    x_i <- signals[[sim_id]]
    if (length(x_i)==0){
      return(numeric(0))
    } else{
      xs <- xs_vals[sim_id]
      return(sort(bid_vec_individual(mu[sim_id], sigma[sim_id], c, lambda[sim_id], k, x_i, n_max)[x_i >= xs], decreasing=TRUE))
    }
  })
  shown_bids <- sapply(bids, function(b_i){
    if(length(b_i) == 0){
      return(numeric(0))
    } else if(length(b_i) == 1){
      return(r)
    } else {
      new_b_i <- b_i
      new_b_i[1] <- b_i[2]
      return(new_b_i)
    }
  }
  )
  default <- simulated_default(
    signals, bids, xs_vals, lambda, mu, sigma, n_max,c,k,r
  )

  return(
    auction_data <- data.frame(
      n_images,
      r,
      xs_vals,
      v,
      'signals'=I(signals),
      'bids'=I(shown_bids),
      'all_bids'=I(bids),
      n=sapply(shown_bids, length),
      'default'=default
    )
  )
}
