#' Prior
#'
#' @param params The parameters
#'
#' @return A boolean indicating whether a parameter vector is assigned non-zero probability under the prior
#' @export
#'
prior <- function(params){
  return(
    1- max(
      c(
        params[3]<=0,
        params[4]<=0,
        params[5]<=0,
        params[6]<=0,
        params[9]<=0,
        params[9]>=1
      )
    )
  )
}
