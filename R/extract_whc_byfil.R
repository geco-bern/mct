## function to extract info by file
extract_whc_byfil <- function(ifil){
  
  ## do it row by row to avoid apparent memory problems
  extract_whc_row <- function(df){
    df %>% 
      tidyr::unnest(data_soiltext_top) %>%
      dplyr::select(lon, lat, fc_top = fc, pwp_top = pwp, whc_top = whc, data_soiltext_sub) %>%
      tidyr::unnest(data_soiltext_sub) %>% 
      dplyr::select(lon, lat, fc_top, pwp_top, whc_top, fc_sub = fc, pwp_sub = pwp, whc_sub = whc)
  }
  load(ifil) # should load 'df_whc'
  df_whc <- df_whc %>% 
    ungroup()
  df <- purrr::map_dfr(as.list(seq(nrow(df_whc))), ~{slice(df_whc, .) %>% extract_whc_row()})
  
  ## old version:
  # df <- df_whc %>% 
  #   dplyr::ungroup() %>% 
  #   tidyr::unnest(data_soiltext_top) %>%
  #   dplyr::select(lon, lat, fc_top = fc, pwp_top = pwp, whc_top = whc, data_soiltext_sub) %>%
  #   tidyr::unnest(data_soiltext_sub) %>% 
  #   dplyr::select(lon, lat, fc_top, pwp_top, whc_top, fc_sub = fc, pwp_sub = pwp, whc_sub = whc)
  
  return(df)
}