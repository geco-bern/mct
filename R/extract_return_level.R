extract_return_level <- function(df, period){
  df %>% 
    dplyr::filter(return_period == period) %>% 
    dplyr::pull(return_level)
}