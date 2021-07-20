get_bal_byilon_lores <- function(ilon){
  
  source("R/get_bal.R")
  
  dirn <- "~/mct/data/df_bal_lores/"
  filn <- paste0("df_bal_ilon_", ilon, ".RData")
  if (!dir.exists(dirn)) system("mkdir -p ~/mct/data/df_bal_lores")
  path_out <- paste0(dirn, filn)

  if (!file.exists(path_out)){
  
    ilon_lores <- ilon
    
    ## Open PET-mm file
    dirn <- "~/data/sofun_outputs/global_FULL_MODIS-C006_MOD15A2_v3.4/data_tidy/"
    filn <- paste0("global_FULL_MODIS-C006_MOD15A2_v3.4.d.pet_ilon_", ilon, ".RData")
    path_pet <- paste0(dirn, filn)
      
    ## open snow file of corresponding longitude slice
    dirn <- "~/mct/data/df_snow/"
    filn <- paste0("df_snow_ilon_", ilon_lores, ".RData")
    path_snow <- paste0(dirn, filn)
    
    if (file.exists(path_pet) && file.exists(path_snow)){
     
      load(path_pet) # loads 'df'
      df_pet <- df
      load(path_snow)  # loads 'df'
      df_snow <- df
      
      ## get closest matching latitude indices and merge data frames
      df <- df_pet %>% 
        
        ## round to correct numerical imprecision on some lon and lat values
        mutate(lon = round(lon, digits = 2), lat = round(lat, digits = 2)) %>% 
        
        ## select only time and pet from alexi dataframe
        mutate(data = purrr::map(data, ~dplyr::select(., time, pet))) %>% 
        
        ## merge watch data into alexi data frame
        left_join(df_snow %>% 
                    mutate(lon = round(lon, digits = 2), lat = round(lat, digits = 2)) %>% 
                    rename(data_snow = data),
                  by = c("lon", "lat")) %>% 
        
        ## select only time and liquid water to soil from watch data frame
        mutate(data_snow = purrr::map(data_snow, ~dplyr::select(., time, liquid_to_soil))) %>% 
        
        ## remove rows where snow data is missing
        ungroup() %>% 
        dplyr::filter(!is.null(data_snow)) %>% 
        
        ## merge liquid into 'data'
        mutate(data = purrr::map2(data, data_snow, ~left_join(.x, .y, by = "time"))) %>% 
        dplyr::select(-data_snow) %>% 
        
        ## interpolate ET, get water balance, and cut NA from head and tail
        # slice(50) %>% 
        dplyr::mutate( data = purrr::map(
          data, 
          ~get_bal(., varnam_bal = "bal", varnam_prec = "liquid_to_soil", varnam_et = "pet"))
        ) 
      
      rlang::inform(paste("Writing file:", path_out))    
      save(df, file = path_out)
      
    } else {
      
      if (!file.exists(path_pet)) rlang::warn(paste0("File missing:", path_pet))
      if (!file.exists(path_snow))  rlang::warn(paste0("File missing:", path_snow))
    
    }
    
  } else {
    rlang::inform(paste("File exists already:", path_out))
  } 
  
  error = 0
  return(error)
}