get_flue_cwdmax <- function(df, nam_lue){

  ## rename
  df <- df %>% rename(lue = !!nam_lue)
  
  if (any(!is.na(df$lue))){
    
    nbin <- 5
    
    df <- df %>%
      mutate(bin = ntile(deficit, nbin))
    
    df_agg <- df %>%
      ungroup() %>%
      group_by(bin) %>%
      summarise(lue = median(lue, na.rm = TRUE))			
    
    ## get fractional reduction in LUE in highest CWD bin, relative to first bin
    flue <- (df_agg %>% slice(nbin) %>% pull(lue)) / (df_agg %>% slice(1) %>%  pull(lue))
    flue <- ifelse(length(flue)==0, NA, flue)
    
    cwdmax <- max(df$deficit, na.rm = TRUE)
    
  } else {
    flue <- NA
    cwdmax <- NA
  }
  
	return(list(flue = flue, cwdmax = cwdmax))

}