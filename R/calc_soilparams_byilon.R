calc_soilparams_byilon <- function(df){
  
  source("R/calc_soilparams.R")
  
  df <- df %>% 
    dplyr::select(fclay = T_CLAY, fgravel = T_GRAVEL, forg = T_OC, fsand = T_SAND, roots = ROOTS) %>% 
    calc_soilparams(., method = "balland") %>% 
    tidyr::nest(data_soiltext_top = everything()) %>% 
    
    bind_cols(
      .,
      df %>% 
        dplyr::select(fclay = S_CLAY, fgravel = S_GRAVEL, forg = S_OC, fsand = S_SAND, roots = ROOTS) %>% 
        calc_soilparams(., method = "balland") %>% 
        tidyr::nest(data_soiltext_sub = everything())
    )
  
  return(df)
}