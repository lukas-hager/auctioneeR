h_inv <- function(mu_1,mu_0,x_i,n,k){
  f <- (1*(n==2) + 2*(n>2))

  return(
    ((n+k)*mu_1-k*mu_0-f*x_i)/(n-f)
  )
}
