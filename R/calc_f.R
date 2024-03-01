#' Helper Function for "Degrees of Freedom" for Number of Bidders
#'
#' If \eqn{n>2}, we want to fix the first and second-order statistics. If \eqn{n=2}, we want to fix only the first-order statistic.
#'
#' @param n Number of bidders.
#'
#' @return The number of "free" bidders.
#' @export
#'

calc_f <- function(n){
  return(1*(n==2) + 2*(n>2))
}
