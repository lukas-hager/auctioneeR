bid_vec_individual <- function(mu,sigma,c,lambda,k,x_i,n_max,interp_grids, x_seq, diff_vals){
  p <- stats::dpois(2:n_max, lambda=lambda)
  f_y <- rbind(sapply(c(2:n_max), function(n_val){f_y_y(x_i, mu, sigma, n_val, k)}))
  weights <- f_y %*% p

  bids <- rbind(
    sapply(
      2:n_max,
      function(n){
        sapply(x_i, function(x){
          z <- (x-mu)/sigma
          interps <- get_moments(z, n, interp_grids, x_seq, diff_vals)
          sigma*bid(z,n=n,c=c,k=k,interps=interps)+mu
        })
      }
    )
  )

  return(as.vector(((bids * f_y) %*% p)/weights))
}
