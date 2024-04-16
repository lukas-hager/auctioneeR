#' Estimate MCMC
#'
#' @param data The auction data
#' @param n_steps The number of draws
#' @param params The vector of parameters
#' @param n_max The maximum number of bidders
#' @param par Whether or not to run the estimation in parallel
#'
#' @return A list of draws and parameters that caused errors.
#' @export
#'
mcmc <- function(data, n_steps, params, n_max, par=FALSE){
  set.seed(42069)
  out_vec <- c()
  errors <- c()
  old_params <- params[c(1:9)]
  old <- evaluate_likelihood_grid(
    data,
    params[1],
    params[2],
    params[3],
    params[4],
    params[5],
    params[6],
    params[7],
    params[8],
    params[9],
    n_max,
    par
  )

  for (step in (1:n_steps)){
    start <- proc.time()
    noise <- stats::rnorm(9, sd=params[c(1:9)]/100)
    new_params <- old_params[c(1:9)] + noise

    if(prior(new_params) == 0){
      print(stringr::str_interp('${step}: Prior is zero'))
      out_vec <- c(out_vec, old_params)
    } else {
      tryCatch({
        new <- evaluate_likelihood_grid(
          data,
          new_params[1],
          new_params[2],
          new_params[3],
          new_params[4],
          new_params[5],
          new_params[6],
          new_params[7],
          new_params[8],
          new_params[9],
          n_max,
          par
        )

        alpha <- min(exp(new-old), 1)
        accept <- stats::runif(1) <= alpha
        if(accept){
          print(stringr::str_interp('${step}: Accepted (${unname((proc.time()-start)["elapsed"])})'))
          old_params <- new_params
          old <- new
          out_vec <- c(out_vec, new_params)
        }else{
          print(stringr::str_interp('${step}: Rejected (${unname((proc.time()-start)["elapsed"])})'))
          out_vec <- c(out_vec, old_params)
        }
      },
      error = function(cond){
        message(conditionMessage(cond))
        errors <- c(errors, new_params)
        rep(NA, length(new_params))
      })
    }
  }
  return(list('draws' = out_vec, 'errors' = errors))
}