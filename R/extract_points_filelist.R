extract_points_filelist <- function(df, list, dirnam = "", fil_pattern = " ", filetype = "watch"){
  
  for (ifil in list[1:2]){
    
    filename <- paste0(dirnam, ifil) # "~/data/watch_wfdei/Rainf_daily/Rainf_daily_WFDEI_CRU_200309.nc"
    rasta <- raster::brick(filename)
    
    if (!("data0" %in% names(df))){
      df <- df %>% 
        dplyr::mutate(V1 = NA) %>% 
        tidyr::nest(V1) %>% 
        dplyr::rename(data0 = data)
    }
    
    ## for watch-wfdei, construct data from file name
    if (filetype=="watch"){
      tmp <- stringr::str_remove(ifil, fil_pattern) %>% 
        stringr::str_remove(., "_") %>% 
        stringr::str_remove(., ".nc")
      year <- tmp %>% 
        stringr::str_sub(., start = 1, end = 4)
      moy <- tmp %>% 
        stringr::str_sub(., start = 5, end = 6)
      timevals <- raster::getZ(rasta)
      datevals <- lubridate::ymd(paste0(year, "-", moy, "-01")) + lubridate::days(timevals - 1)
      
    } else {
      datevals <- raster::getZ(rasta)
      datevals <- lubridate::ymd(datevals)
    }
    
    ## extract values from file and append it to 'data' (separate df for each point)
    df <- raster::extract(rasta, sp::SpatialPoints(dplyr::select(df, lon, lat)), sp = TRUE) %>% 
      as_tibble() %>% 
      tidyr::nest(-lon, -lat) %>%
      right_join(df, by = c("lon", "lat")) %>%
      dplyr::mutate(data = purrr::map(data, ~dplyr::slice(., 1)) ) %>% 
      dplyr::mutate(data = purrr::map(data, ~t(.))) %>% 
      dplyr::mutate(data = purrr::map(data, ~as_tibble(.))) %>% 
      dplyr::mutate(data = purrr::map(data, ~bind_cols(., tibble(date = datevals)))) %>% 
      dplyr::mutate(data = purrr::map2(data0, data, ~bind_rows(.x, .y))) %>% 
      dplyr::select(-data0) %>% 
      dplyr::rename(data0 = data)
      # dplyr::mutate(data0 = purrr::map(data0, ~drop_na(.)))
    
  }
  return(df)
}