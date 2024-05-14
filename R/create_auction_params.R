#' Add the implied auction parameters to the dataset
#'
#' @param auction_data The item characteristics
#' @param L_INT The intercept for lambda
#' @param L_N_IMAGE The coefficient on images for lambda
#' @param L_YEAR The coefficient on year for lambda
#' @param MU_INT The intercept for mu
#' @param MU_N_IMAGE The coefficient on images for lambda
#' @param MU_YEAR The coefficient on year for lambda
#' @param SIGMA_INT The intercept for sigma
#' @param SIGMA_N_IMAGE The coefficient for images for sigma
#' @param SIGMA_YEAR The coefficient for year for sigma
#'
#' @return A dataframe with all the auction parameters
#' @export
#'
create_auction_params <- function(auction_data,L_INT,L_N_IMAGE,L_YEAR,MU_INT,MU_N_IMAGE,MU_YEAR,SIGMA_INT,SIGMA_N_IMAGE,SIGMA_YEAR){
  auction_data_iter <- auction_data %>%
    dplyr::mutate(lambda_t = exp(L_INT + L_N_IMAGE*auction_data$n_images + L_YEAR*auction_data$year),
                  mu_t = MU_INT + MU_N_IMAGE*auction_data$n_images + MU_YEAR*auction_data$year,
                  sigma_t = exp(SIGMA_INT + SIGMA_N_IMAGE*auction_data$n_images + SIGMA_YEAR*auction_data$year))
  return(auction_data_iter)
}
