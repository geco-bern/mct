extract_points_filelist <- function(df, list, varnam, dirnam = "", fil_pattern = " ", filetype = "watch", use_nco = FALSE){
  
  # ## xxx debug
  # df <- df %>% slice(50:51)
  
  for (ifil in list){
    
    filename <- paste0(dirnam, ifil) # "~/data/watch_wfdei/Rainf_daily/Rainf_daily_WFDEI_CRU_200309.nc"
    print(paste0("Opening file ", filename, " as raster brick ..."))
    rasta <- raster::brick(filename, varname = varnam)
    
    if (!("data0" %in% names(df))){
      df <- df %>% 
        dplyr::mutate(V1 = NA) %>% 
        tidyr::nest(data0 = V1)
    }
    
    ## for watch-wfdei, construct data from file name
    if (filetype=="watch"){
      tmp <- stringr::str_replace(ifil, fil_pattern, "") %>% 
        stringr::str_replace(., "_", "") %>% 
        stringr::str_replace(., ".nc", "")
      year <- tmp %>% 
        stringr::str_sub(., start = 1, end = 4)
      moy <- tmp %>% 
        stringr::str_sub(., start = 5, end = 6)
      timevals <- raster::getZ(rasta)
      datevals <- lubridate::ymd(paste0(year, "-", moy, "-01")) + lubridate::days(timevals - 1)
      
    } else if (filetype == "landflux") {
      datevals <- raster::getZ(rasta)
      datevals <- lubridate::ymd(datevals) - lubridate::days(1) # file of year 1984 contains 1 jan 85, but not 1 jan 84. seems to be shifted by one
    } else {
      datevals <- raster::getZ(rasta)
      datevals <- lubridate::ymd(datevals)
    }

    # if (use_nco){
    #   vec <- extract_point_nc( df$lon, df$lat, filename, varnam )
    # }
        
    ## extract values from file and append it to 'data' (separate df for each point)
    df <- raster::extract(rasta, sp::SpatialPoints(dplyr::select(df, lon, lat)), sp = TRUE) %>% 
      as_tibble() %>% 
      tidyr::nest(data = c(-lon, -lat)) %>%
      right_join(df, by = c("lon", "lat")) %>%
      dplyr::mutate(data = purrr::map(data, ~dplyr::slice(., 1)) ) %>% 
      dplyr::mutate(data = purrr::map(data, ~t(.))) %>% 
      dplyr::mutate(data = purrr::map(data, ~as_tibble(.))) %>% 
      dplyr::mutate(data = purrr::map(data, ~bind_cols(., tibble(date = datevals)))) %>% 
      dplyr::mutate(data = purrr::map2(data0, data, ~bind_rows(.x, .y))) %>% 
      dplyr::select(-data0) %>% 
      dplyr::rename(data0 = data) %>% 
      dplyr::mutate(data0 = purrr::map(data0, ~drop_na(., date)))
    
  }
  return(df)
}