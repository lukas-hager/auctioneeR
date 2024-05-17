#' Creates a dataframe of simulations
#'
#' @param k The coefficient on the signal standard deviation
#' @param n_sims The number of simulations
#'
#' @return A datatable of simulations
#' @export
#'
create_sim_df <- function(k, n_sims = 100){
  set.seed(42069)

  n_bidders <- rep(c(1:30), n_sims)
  eps_i <- rnorm(n = sum(n_bidders), mean=0, sd = sqrt(k))
  v_i <- rep(rnorm(n=length(n_bidders), mean=0, sd=1), n_bidders)
  x_i <- v_i + eps_i
  n <- rep(n_bidders, n_bidders)
  dt = data.table(
    v_i,
    x_i,
    eps_i,
    n
  )
}
