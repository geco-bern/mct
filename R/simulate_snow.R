simulate_snow <- function(df, varnam_prec = NULL, varnam_snow = NULL, varnam_temp = NULL){
  
  ## This is for daily input data!

  ## initialise
  out_snow_pool <- rep(NA, nrow(df))
  out_liquid_to_soil <- rep(NA, nrow(df))
  
  calc_fsnow <- function(temp){ 
    max( min( 1.0,  1.0 - ( 1.0 / 2.0 ) * temp ), 0.0 ) 
    }
  
  simulate_snow_tstep <- function(prec, snow, temp, snow_pool){
  
    ## fixed parameters
    temp_threshold <- 1.0
    maxmeltrate <- 1.0
    
    if ( snow_pool > 0.0 && temp > temp_threshold ){
      melt  <- min( snow_pool, maxmeltrate * ( temp - temp_threshold ) )
    } else {
      melt <- 0.0
    }

    snow_pool <- snow_pool + snow - melt
    liquid_to_soil <- prec + melt
    
    return(list(liquid_to_soil = liquid_to_soil, snow_pool = snow_pool))
  }
  
  ## data for spinup: first 1 year
  df_spinup <- df %>% 
    mutate(year = lubridate::year(date)) %>% 
    dplyr::filter(year == min(.$year, na.rm = TRUE))
  
  snow_pool <- 0.0
  
  ## make nice names
  varnam_prec <- enquo(varnam_prec)
  df <- df %>% 
    dplyr::rename(prec = !!varnam_prec)
  
  if (is.null(varnam_snow)){
    ##----------------------------
    ## Snow rate is NOT given -> use calc_fsnow()
    ##----------------------------
    varnam_temp <- enquo(varnam_temp)
    df <- df %>% 
      dplyr::rename(temp = !!varnam_temp)
    
    df <- df %>% 
      rowwise() %>% 
      mutate(fsnow = calc_fsnow(temp)) %>% 
      mutate(snow = fsnow * prec) %>% 
      mutate(prec = prec - snow)
  
  } else {
    
    varnam_snow <- enquo(varnam_snow)
    df <- df %>% 
      dplyr::rename(snow = !!varnam_snow)
    
  }

  ##----------------------------
  ## Spinup
  ##----------------------------
  for (doy in 1:365){
    ## get daily change in snow pool and meltwater
    out <- simulate_snow_tstep(
      prec = df[["prec"]][doy],
      snow = df[["snow"]][doy],
      temp = df[["temp"]][doy],
      snow_pool = snow_pool
    )
    
    ## update snow pool
    snow_pool <- out$snow_pool
  } 
  
  ##----------------------------
  ## transient simulation
  ##----------------------------
  for (doy in 1:nrow(df)){
    ## get daily change in snow pool and meltwater
    out <- simulate_snow_tstep(
      prec = df[["prec"]][doy],
      snow = df[["snow"]][doy],
      temp = df[["temp"]][doy],
      snow_pool = snow_pool
    )
    
    ## update snow pool
    snow_pool <- out$snow_pool
    
    ## record output variables
    out_snow_pool[doy] <- snow_pool
    out_liquid_to_soil[doy] <- out$liquid_to_soil
  } 
  
  
  ## add to data frame
  df$snow_pool <- out_snow_pool
  df$liquid_to_soil <- out_liquid_to_soil
  
  return(df)
}

