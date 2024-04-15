bid_vec_individual <- function(mu,sigma,c,lambda,k,x_i,n_max){
  p <- stats::dpois(2:n_max, lambda=lambda)
  f_y <- rbind(sapply(c(2:n_max), function(n_val){f_y_y(x_i, mu, sigma, n_val, k)}))
  weights <- f_y %*% p

  bids <- rbind(
    sapply(
      2:n_max,
      function(n){
        sapply(x_i, bid, n=n,c=c,k=k,mu_0=mu,sigma_0=sigma)
      }
    )
  )

  return(as.vector(((bids * f_y) %*% p)/weights))
}
