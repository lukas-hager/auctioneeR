#' Estimate the MCMC Model
#'
#' @param data The dataset of auction data
#' @param n_steps The number of MCMC draws to do
#' @param start_params The starting parameters for the MCMC routine.
#' @param n_max The maximum number of bidders.
#' @param par Whether or not to run the estimation in parallel
#'
#' @return The `draws` object from \code{\link{mcmc}}
#' @export
#'
estimate_model <- function(data, n_steps, start_params, n_max, par=FALSE, n_cores=parallel::detectCores() - 2){
  if(par){
    # create the cluster
    my_cluster <- parallel::makeCluster(
      n_cores,
      type = "FORK"
    )

    doParallel::registerDoParallel(cl = my_cluster)
    parallel::clusterEvalQ(my_cluster, {
      library(akima)
    })
  }

  draws <- mcmc(data, n_steps, start_params, n_max, par)

  if(par){
    parallel::stopCluster(my_cluster)
  }

  return(draws)
}
