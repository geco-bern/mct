get_bal_byilon_lores <- function(ilon, config = read_input_config()){
  
  source("R/get_bal.R")
  
  path_out <- climate_output_path(
    paste0("data/df_bal_lores/df_bal_ilon_", ilon, ".RData"),
    config
  )
  ensure_directory(dirname(path_out))

  if (!file.exists(path_out)){
  
    ilon_lores <- nearest_source_index(
      ilon,
      from_source = config$et$low_resolution_source,
      to_source = config$precipitation$rain
    )
    
    ## Open configured low-resolution ET file converted to mm d-1.
    path_et_mm <- climate_output_path(
      paste0("data/df_et_mm_lores/df_et_mm_ilon_", ilon, ".RData"),
      config
    )
      
    ## open snow file of corresponding longitude slice
    path_snow <- climate_output_path(
      paste0("data/df_snow/df_snow_ilon_", ilon_lores, ".RData"),
      config
    )
    
    if (file.exists(path_et_mm) && file.exists(path_snow)){
     
      load(path_et_mm) # loads 'df_alexi'
      df_pet <- df_alexi
      load(path_snow)  # loads 'df'
      df_snow <- df
      coordinate_digits <- source_coordinate_digits(
        config$et$low_resolution_source,
        "longitude"
      )
      
      ## get closest matching latitude indices and merge data frames
      df <- df_pet %>% 
        
        ## round to correct numerical imprecision on some lon and lat values
        mutate(lon = round(lon, digits = coordinate_digits), lat = round(lat, digits = coordinate_digits)) %>%
        
        ## select only time and configured ET from the converted dataframe
        mutate(data = purrr::map(data, ~dplyr::select(., time, et_mm))) %>%
        
        ## merge watch data into alexi data frame
        left_join(df_snow %>% 
                    mutate(lon = round(lon, digits = coordinate_digits), lat = round(lat, digits = coordinate_digits)) %>%
                    rename(data_snow = data),
                  by = c("lon", "lat")) %>% 
        
        ## select only time and liquid water to soil from watch data frame
        mutate(data_snow = purrr::map(data_snow, ~dplyr::select(., time, liquid_to_soil))) %>% 
        
        ## remove rows where snow data is missing
        ungroup() %>% 
        dplyr::filter(!purrr::map_lgl(data_snow, is.null)) %>%
        
        ## merge liquid into 'data'
        mutate(data = purrr::map2(data, data_snow, ~left_join(.x, .y, by = "time"))) %>% 
        dplyr::select(-data_snow) %>% 
        
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
