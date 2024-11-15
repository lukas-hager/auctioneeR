#' MLE Objective Function
#'
#' @param param_vec The vector of parameters
#' @param auction_data The data
#' @param par Whether the routine should be run in parallel
#'
#' @return The likelihood
#' @export
#'
mle_obj <- function(param_vec, auction_data){
  tryCatch(
    {
      out <- evaluate_likelihood_grid(
        auction_data,
        param_vec[1],
        param_vec[2],
        param_vec[3],
        param_vec[4],
        param_vec[5],
        param_vec[6],
        param_vec[7],
        param_vec[8],
        param_vec[9],
        param_vec[10],
        param_vec[11],
        param_vec[12],
        30,
        par=TRUE
      )

      # optim doesn't like NaN or -Inf
      clipped_val <- -max(out, -100000)
      print(dplyr::if_else(is.nan(clipped_val)|is.na(clipped_val), 100000, clipped_val))
      return(dplyr::if_else(is.nan(clipped_val)|is.na(clipped_val), 100000, clipped_val))
    },
    error = function(cond) {
      print(100000)
      return(100000)
    }
  )
}
