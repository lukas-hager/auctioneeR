#' Assess the output of the model
#'
#' @param df_fp A filepath for a excel sheet of draws
#'
#' @return A ggplot of the parameters
#' @export
#'
assessment <- function(df_fp){
  df <- read_csv(df_fp, show_col_types = FALSE)
  df_long <- df %>%
    mutate(id = row_number()) %>%
    pivot_longer(cols = c(MU_INT:D_R))

  print(
    df_long %>%
      ggplot(data = .) +
      geom_line(aes(x=id,y=value))+
      facet_wrap(~name, scales='free')
  )

  return(
    df_long %>%
      group_by(name) %>%
      summarise(mean = mean(value, na.rm=TRUE),
                median = median(value, na.rm=TRUE),
                sd = sd(value, na.rm=TRUE))
  )
}


