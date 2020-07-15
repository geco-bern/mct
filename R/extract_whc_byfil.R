## function to extract info by file
extract_whc_byfil <- function(ifil){
  load(ifil) # should load 'df_whc'
  df <- df_whc %>% 
    dplyr::ungroup() %>% 
    tidyr::unnest(data_soiltext_top) %>% 
    dplyr::select(lon, lat, fc_top = fc, pwp_top = pwp, whc_top = whc, data_soiltext_sub) %>% 
    tidyr::unnest(data_soiltext_sub) %>% 
    dplyr::select(lon, lat, fc_top, pwp_top, whc_top, fc_sub = fc, pwp_sub = pwp, whc_sub = whc)
  return(df)
}