extract_return_level <- function(out, period){
  if (any(!is.na(out$df_return$return_period))){
    out$df_return %>% 
      dplyr::filter(return_period == period) %>% 
      dplyr::pull(return_level)
  } else {
    NA
  }
}