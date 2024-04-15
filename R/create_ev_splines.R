create_ev_splines <- function(k, n_max){
  x_seq <- seq(-5,5, length.out=20)
  spline_list <- lapply(c(1:n_max), function(n_val){
    y = sapply(x_seq, function(x){
      riemann_sums(
        f=num,
        lower=-10,
        upper=10,
        n_bins=100,
        x=x,
        n=n_val,
        mu_0=0,
        sigma_0=1,
        k=k
      ) / riemann_sums(
        f=den,
        lower=-10,
        upper=10,
        n_bins=100,
        x=x,
        n=n_val,
        mu_0=0,
        sigma_0=1,
        k=k
      )
    })
    return(splinefun(x_seq, y, 'natural'))
  })
  return(spline_list)
}
