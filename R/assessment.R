#' Assess the output of the model
#'
#' @param df_fp A filepath for a excel sheet of draws
#' @param burn_in The size of initial samples to throw away.
#' @param table Whether or not to return the data
#'
#' @return A ggplot of the parameters
#' @export
#'
assessment <- function(df_fp, burn_in=10000, table=TRUE){
  df <- readr::read_csv(df_fp, show_col_types = FALSE)
  df_long <- df %>%
    dplyr::mutate(id = row_number()) %>%
    dplyr::filter(id > burn_in) %>%
    tidyr::pivot_longer(cols = c(MU_INT:D_R))

  if(!table){
    print(
      df_long %>%
        ggplot2::ggplot(data = .) +
        ggplot2::geom_line(aes(x=id,y=value), color = 'dodgerblue3') +
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::theme_bw() +
        ggplot2::facet_wrap(~name, scales='free', nrow = 4, ncol = 3)
    )
  }else{
    return(
      df_long %>%
        dplyr::group_by(name) %>%
        dplyr::summarise(mean = mean(value, na.rm=TRUE),
                         median = median(value, na.rm=TRUE),
                         sd = sd(value, na.rm=TRUE)) %>%
        dplyr::mutate(t_stat = mean/sd,
                      sig = abs(t_stat) > 2)
    )
  }
}


