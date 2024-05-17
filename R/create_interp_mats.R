#' Create the matrices for interpolation
#'
#' @param k The coefficient on the standard deviation
#' @param n_max The maximum number of bidders
#' @param x_vals The sequence of signals
#' @param diff_vals The sequence of values to "look back" from a signal
#'
#' @return A list of interpolation matrices for each number of bidders
#' @export
#' @importFrom data.table :=
#'
create_interp_mats <- function(k,n_max,x_vals,diff_vals){
  n_signals <- c(2:n_max)
  v_vals <- seq(-5,5,length.out=50)

  df = data.table::CJ('x_i'=x_vals, 'diff'=diff_vals)[
    data.table::CJ('x_i'=x_vals, 'v'=v_vals), on='x_i', allow.cartesian=TRUE
  ][
    data.table::CJ('x_i'=x_vals, 'n_signals'=n_signals), on='x_i', allow.cartesian=TRUE
  ][
    , p := f(v, x_i+diff, x_i, n_signals, k)
  ][
    , .('cum_p' = sum(p)), by = c('x_i', 'diff', 'n_signals')
  ][
    , 'F' := cumsum(cum_p)/sum(cum_p), by = c('x_i', 'n_signals')
  ][
    order(diff,decreasing=TRUE), 'E' := cumsum(cum_p*(x_i+diff))/cumsum(cum_p), by = c('x_i', 'n_signals')
  ]

  dfwide = data.table::dcast(df, n_signals + x_i ~ diff, value.var = "F")

  f_mats = lapply(n_signals, function(n){
    d = data.table::copy(dfwide)
    as.matrix(d[n==n_signals][,c('n_signals', 'x_i'):=NULL])
  })

  dfwide2 = data.table::dcast(df, n_signals + x_i ~ diff, value.var = "E")

  e_mats = lapply(n_signals, function(n){
    d2 = data.table::copy(dfwide2)
    as.matrix(d2[n==n_signals][,c('n_signals', 'x_i'):=NULL])
  })

  return(list('f_mats'=f_mats,'e_mats'=e_mats))
}
