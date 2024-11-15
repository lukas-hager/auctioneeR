#' MLE
#'
#' @param init_params The initial parameter vector
#' @param auction_data The data over which to run the optimization
#'
#' @return The optimization object
#' @export
#'
mle <- function(init_params, auction_data){
  optim(
    par=init_params,
    fn=mle_obj,
    auction_data=auction_data,
    lower = c(0, 0, -Inf, 0, 0, -Inf, 0, 0, -Inf, 0, 0, 0),
    upper = c(Inf, Inf, 0, Inf, Inf, 0, Inf, Inf, 0, 1, Inf, 1),
    method = 'L-BFGS-B',
    control = list('lmm'=100, 'trace'=6)
  )
}
