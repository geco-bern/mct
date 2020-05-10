extract_return_level <- function(out, period){
  out$df_return %>% 
    dplyr::filter(return_period == period) %>% 
    dplyr::pull(return_level)
}