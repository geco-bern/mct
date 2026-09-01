get_bal_byilon <- function(ilon_hires, config = read_input_config()){
  
  source("R/get_bal.R")
  
  path_out <- climate_output_path(
    paste0("data/df_bal/df_bal_ilon_", ilon_hires, ".RData"),
    config
  )
  ensure_directory(dirname(path_out))

  if (!file.exists(path_out)){
  
    ilon_lores <- nearest_source_index(
      ilon_hires,
      from_source = config$et$source,
      to_source = config$precipitation$rain
    )
    
    ## Open ET-mm file
    path_et_mm <- climate_output_path(
      paste0("data/df_et_mm/df_et_mm_ilon_", ilon_hires, ".RData"),
      config
    )
    
    ## open snow file of corresponding longitude slice
    path_snow <- climate_output_path(
      paste0("data/df_snow/df_snow_ilon_", ilon_lores, ".RData"),
      config
    )
    
    if (file.exists(path_et_mm) && file.exists(path_snow)){
     
      load(path_et_mm) # loads 'df_alexi'
      load(path_snow)  # loads 'df'
      et_digits <- source_coordinate_digits(config$et$source, "longitude")
      forcing_digits <- source_coordinate_digits(config$precipitation$rain, "longitude")
      df_watch <- df %>% 
        mutate(
          lon = round(lon, digits = forcing_digits),
          lat = round(lat, digits = forcing_digits)
        ) # rename
      rm("df")
      
      ## get closest matching latitude indices and merge data frames
      df <- df_alexi %>% 
        
        ## round to correct numerical imprecision on some lon and lat values
        mutate(lon = round(lon, digits = et_digits), lat = round(lat, digits = et_digits)) %>%
        
        ## select only time and et_mm from alexi dataframe
        mutate(data = purrr::map(data, ~dplyr::select(., time, et_mm))) %>% 
        
        ## merge watch data into alexi data frame
        left_join(df_watch %>% 
                    rename(lon_lores = lon, lat_lores = lat, data_watch = data),
                  by = c("lon_lores", "lat_lores")) %>% 
        
        ## select only time and liquid water to soil from watch data frame
        mutate(data_watch = purrr::map(data_watch, ~dplyr::select(., time, liquid_to_soil))) %>% 
        
        ## drop columns no longer used
        dplyr::select(-dplyr::any_of(c("lon_lores", "lat_lores", "elv"))) %>%
        
        ## remove rows where watch data is missing
        ungroup() %>% 
        dplyr::filter(!purrr::map_lgl(data_watch, is.null)) %>%
        
        ## merge liquid into 'data'
        mutate(data = purrr::map2(data, data_watch, ~left_join(.x, .y, by = "time"))) %>% 
        dplyr::select(-data_watch) %>% 
        
        ## interpolate ET, get water balance, and cut NA from head and tail
        # slice(50) %>% 
        dplyr::mutate( data = purrr::map(
          data, 
          ~get_bal(., varnam_bal = "bal", varnam_prec = "liquid_to_soil", varnam_et = "et_mm"))
        ) 
      
      rlang::inform(paste("Writing file:", path_out))    
      save(df, file = path_out)
      
    } else {
      
      if (!file.exists(path_et_mm)) rlang::warn(paste0("File missing:", path_et_mm))
      if (!file.exists(path_snow))  rlang::warn(paste0("File missing:", path_snow))
    
    }
    
  } else {
    rlang::inform(paste("File exists already:", path_out))
  } 
  
  error = 0
  return(error)
}
