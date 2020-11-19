complement_cwdx <- function(uselon, df_missing_lat){

  ## identify which longitude slice to data from
  lon_hires <- seq(-179.975, 179.975, by = 0.05)
  ilon_hires <- which.min(abs(uselon - lon_hires))
  
  ## read bal data
  filn <- paste0("~/mct/data/df_bal/df_bal_ilon_", as.character(ilon_hires), ".RData")
  
  if (file.exists(filn)){
    
    load(filn)  # reads 'df'
    
    ## try getting CWDX again
    df_cwdx_corr <- df %>%
      
      ## re-calculate only for missing
      dplyr::filter(lat %in% df_missing_lat$lat) %>% 
      
      ## re-calculate
      dplyr::mutate(
        out_mct_corr = purrr::map(
          data,
          ~get_plantwhc_mct_bysite(
            .,
            varname_wbal = "bal",
            varname_date = "time",
            thresh_terminate = 0,
            thresh_drop = 0.9,
            fittype = "Gumbel"))
      ) %>% 
      
      ## drop data frame from 'bal'
      dplyr::select(-data)
    
    ## complement cwdx dataframe with corrected one
    dirn <- "~/mct/data/df_cwdx/"
    filn <- paste0("df_cwdx_ilon_", as.character(ilon_hires), ".RData")
    path <- paste0(dirn, filn)
    load(path)  # loads 'df'
    
    df <- df %>% 
      left_join(df_cwdx_corr, by = c("lon", "lat")) %>% 
      rowwise() %>% 
      mutate(out_mct = ifelse((lat %in% df_missing_lat$lat) & !is.na(out_mct_corr), out_mct_corr, out_mct)) %>% 
      dplyr::select(-out_mct_corr)
    
    ## overwrite file
    print(paste("Overwriting file:", path))
    save(df, file = path) 
  }
  
}