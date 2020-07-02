get_bal <- function(df, varnam_bal, varnam_et, varnam_prec){
  
  if (sum(!is.na(df[[varnam_et]]))>1){
    
    ## Get mean seasonal cycle to fill gaps in ET and precip
    df_meandoy <- df %>% 
      mutate(doy = lubridate::yday(time)) %>% 
      dplyr::select(doy, !!varnam_et, !!varnam_prec) %>% 
      rename(et = !!varnam_et, prec = !!varnam_prec) %>% 
      group_by(doy) %>% 
      summarise(et_meandoy = mean(et, na.rm = TRUE), 
                prec_meandoy = mean(prec, na.rm = TRUE),
                .groups = 'drop') %>% 
      mutate(et_meandoy = ifelse(is.nan(et_meandoy), NA, et_meandoy),
             prec_meandoy = ifelse(is.nan(prec_meandoy), NA, prec_meandoy))

    df <- df %>% 
      dplyr::select(time, !!varnam_et, !!varnam_prec) %>% 
      rename(et = !!varnam_et, prec = !!varnam_prec) %>% 
      mutate(doy = lubridate::yday(time)) %>% 
      left_join(df_meandoy, by = "doy") %>% 
      mutate(et = ifelse(is.na(et), et_meandoy, et),
             prec = ifelse(is.na(prec), prec_meandoy, prec))
      
    ## For remaining gaps, linearly interpolate ET and assume zero precip
    df$et <- rbeni::myapprox(df$et)
    df$prec[is.na(df$prec)] <- 0.0
    
    ## calculate daily water balance
    df[[varnam_bal]] <- df$prec - df$et
    
    ## retain only time and bal
    df <- df %>% 
      dplyr::select(time, !!varnam_bal)

    ## remove NAs at head and tail (if no gapfilling or interpolation is possible)
    if (any(is.na(df[[varnam_bal]]))){
      df <- rbeni::cutna_headtail_df(df, varnam_bal)
    }
    
  } else {

#     df <- df %>% 
#       rename(et = !!varnam_et, prec = !!varnam_prec) %>% 
#       mutate(et = NA)
    df <- NA
  }
  
  return(df)
}