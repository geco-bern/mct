extract_points_filelist_tidync <- function(df, list, varnam, dlon, lat, dirnam = "", fil_pattern = " ", filetype = "watch"){
  
  ## for explanations, see https://ropensci.org/blog/2019/11/05/tidync/
  
  # ## xxx debug
  # df <- df %>% slice(50:51)
  
  ## get time units
  basedate <- ncmeta::nc_atts(filename, "time") %>% 
    tidyr::unnest(cols = c(value)) %>% 
    dplyr::filter(name == "units") %>% 
    pull(value) %>% 
    stringr::str_remove("days since ") %>% 
    stringr::str_remove(" 00:00:00") %>% 
    lubridate::ymd()
  
  extract_points_bysite <- function(lon, lat, nc, dlon, dlat, basedate){
    ## use user-specified resolution to extract value at coordinate
    nc %>% 
      hyper_filter(lon = lon > lon-dlon & lon < lon+dlon, lat = lat > lat-dlat & lat < lat+dlat) %>% 
      hyper_tibble() %>% 
      rename(lon_read = lon, lat_read = lat) %>% 
      mutate(time = basedate + lubridate::days(time)) %>% 
      group_by(lon_read, lat_read) %>% 
      tidyr::nest()
  }
  
  for (ifil in list){
    
    filename <- paste0(dirnam, ifil) # "~/data/watch_wfdei/Rainf_daily/Rainf_daily_WFDEI_CRU_200309.nc"
    print(paste0("Opening file ", filename, " with tidync ..."))
    
    nc <- tidync::tidync(filename)
    
    df <- df %>% 
      dplyr::select(lon, lat) %>%
      mutate(data = purrr::map2(lon, lat, ~extract_points_bysite(.x, .y, nc, dlon, dlat, basedate))) %>% 
      tidyr::unnest(data)
      
    
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