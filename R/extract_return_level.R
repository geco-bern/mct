extract_return_level <- function(out, object, period){
  
  df %>% 
    dplyr::filter(return_period == period) %>% 
    dplyr::pull(return_level)
}