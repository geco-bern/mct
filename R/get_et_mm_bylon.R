get_et_mm_byilon <- function(ilon_hires){
  
  convert_temp_watch <- function(x){ x - 273.15 }  # K -> degC
  
  convert_et_MJ <- function(x){ x * 1e6 }  # MJ m-2 d-1 -> J m-2 d-1
  
  find_lat_lores <- function(lat_hires, vec_lat_lores){
    vec_lat_lores[which.min(abs(lat_hires - vec_lat_lores))]
  }
  
  source("R/convert_et.R")
  
  ## determine file name
  dirn <- "~/mct/data/df_alexi_et_mm/"
  filn <- paste0("df_alexi_et_mm_ilon_", ilon_hires, ".RData")
  if (!dir.exists(dirn)) system("mkdir -p ~/mct/data/df_alexi_et_mm")
  path <- paste0(dirn, filn)
  
  if (!file.exists(path)){
   
    ## determine closest longitude in 0.5 res files (WATCH)
    lon_lores <- seq(-179.75, 179.75, by = 0.5)
    lon_hires <- seq(-179.975, 179.975, by = 0.05)
    ilon_lores <- which.min(abs(lon_lores - lon_hires[ilon_hires]))
    
    ## Open files (ET from ALEXI-TIR)
    load(paste0("~/data/alexi_tir/data_tidy/EDAY_CERES__ilon_", ilon_hires, ".RData"))
    df_alexi <- df %>% 
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3))
    rm("df")
    
    ## and WATCH-WFDEI temperature data of corresponding longitude slice
    load(paste0("~/data/watch_wfdei/data_tidy/Tair_daily_WFDEI__ilon_", ilon_lores, ".RData"))
    df_watch <- df %>% 
      mutate(lon = round(lon, digits = 2), lat = round(lat, digits = 2))
    rm("df")
    
    ## get elevation data from ETOPO1 for this longitude slice
    df_elv <- rbeni::extract_pointdata_allsites("~/data/etopo/ETOPO1_Bed_g_gef_0.05deg_STANDARD.nc", 
                                         dplyr::select(df_alexi, lon, lat),
                                         time = FALSE
                                         ) %>% 
      rename(elv = ETOPO1_Bed_g_geotiff) %>% 
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3))
    
    ## filter watch data to years within ALEXI data availability (2003-2017)
    df_watch <- df_watch %>% 
      ungroup() %>% 
      dplyr::filter(!is.null(data)) %>% 
      mutate(data = purrr::map(data, ~dplyr::filter(., lubridate::year(time)>2002 & lubridate::year(time)<2018))) %>% 
      mutate(data = purrr::map(data, ~rename(., temp = Tair))) %>% 
      
      ## convert units of temp
      dplyr::mutate(data = purrr::map(data, ~mutate(., temp = convert_temp_watch(temp)))) 
    
    ## get closest matching latitude indices and merge data frames ! 
    ## XXX WRONG!!! THIS TAKES THE NEXT CELL INSTEAD OF CONSIDERING IT NA IF THE CORRESPONDING CELL IS NOT AVAILABLE IN WATCH DATA
    ## vec_lat_lores <- df_watch$lat %>% unique()
    
    ## This is correct
    vec_lat_lores <- seq(-89.75, 89.75, by = 0.5)
    
    df_alexi <- df_alexi %>%

      ## determine the corresponding low-resolution latitude value and join by it
      mutate(lat_lores = purrr::map_dbl(lat, ~find_lat_lores(., vec_lat_lores = vec_lat_lores))) %>% 
      left_join(df_watch %>% 
                  rename(lon_lores = lon, lat_lores = lat, data_watch = data),
                by = "lat_lores") %>% 
      
      ## filter out all ocean/ice cells with no data from watch
      dplyr::filter(!is.na(lon_lores)) %>% 
      
      mutate(data = purrr::map2(data, data_watch, ~right_join(.x, .y, by = "time"))) %>% 
      dplyr::select(-data_watch) %>% 
      
      ## wrap elevation into data frames
      left_join(df_elv, by = c("lon", "lat")) %>% 
      mutate(data = purrr::map(data, ~mutate(., elv))) %>% 

      ## convert units
      dplyr::mutate(data = purrr::map(data, ~mutate(., et = convert_et_MJ(et)))) %>%  
      dplyr::mutate(data_et_mm = purrr::map(data, ~convert_et(.$et, .$temp, .$elv, return_df = TRUE))) %>% 
      dplyr::mutate(data = purrr::map2(data, data_et_mm, ~bind_cols(.x, .y))) %>% 
      dplyr::select(-data_et_mm)
    
    print(paste("Writing file", path))
    save(df_alexi, file = path)
    
  } else {
    print(paste("File exists already: ", path))
  }
  
  error = 0
  return(error)
}