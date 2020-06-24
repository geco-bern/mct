simulate_snow_byilon <- function(ilon){
  
  source("R/simulate_snow2.R")

  convert_temp_watch <- function(x){ x - 273.15 }  # K -> degC
  convert_prec_watch <- function(x){ x * 60 * 60 * 24 }  # kg/m2/s -> mm/day

  dirn <- "~/mct/data/df_snow/"
  filn <- paste0("df_snow_ilon_", ilon, ".RData")
  if (!dir.exists(dirn)) system("mkdir -p ~/mct/data/df_snow")
  path <- paste0(dirn, filn)

  if (!file.exists(path)){
   
    ## get all WATCH-WFDEI data of corresponding longitude slice
    load(paste0("~/data/watch_wfdei/data_tidy/Tair_daily_WFDEI__ilon_", ilon, ".RData"))
    df_temp <- df
    rm("df")
    
    load(paste0("~/data/watch_wfdei/data_tidy/Rainf_daily_WFDEI_CRU__ilon_", ilon, ".RData"))
    df_prec <- df
    rm("df")
    
    load(paste0("~/data/watch_wfdei/data_tidy/Snowf_daily_WFDEI_CRU__ilon_", ilon, ".RData"))
    df_snow <- df
    rm("df")
    
    ## filter watch data to years within ALEXI data availability (2003-2017)
    df_temp <- df_temp %>% 
      ungroup() %>% 
      mutate(data = purrr::map(data, ~dplyr::filter(., lubridate::year(time)>2002 & lubridate::year(time)<2018))) %>% 
      mutate(data = purrr::map(data, ~rename(., temp = Tair))) %>% 
      ## convert units of temp
      dplyr::mutate(data = purrr::map(data, ~mutate(., temp = convert_temp_watch(temp))))
    
    df_prec <- df_prec %>% 
      ungroup() %>% 
      mutate(data = purrr::map(data, ~dplyr::filter(., lubridate::year(time)>2002 & lubridate::year(time)<2018))) %>% 
      mutate(data = purrr::map(data, ~rename(., prec = Rainf))) %>% 
      ## convert units of prec
      dplyr::mutate(data = purrr::map(data, ~mutate(., prec = convert_prec_watch(prec)))) 
    
    df_snow <- df_snow %>% 
      ungroup() %>% 
      mutate(data = purrr::map(data, ~dplyr::filter(., lubridate::year(time)>2002 & lubridate::year(time)<2018))) %>% 
      mutate(data = purrr::map(data, ~rename(., snow = Snowf))) %>% 
      ## convert units of snow
      dplyr::mutate(data = purrr::map(data, ~mutate(., snow = convert_prec_watch(snow)))) 
    
    ## Merge all three variables
    df <- df_temp %>% 
      
      ## merge prec 
      left_join(df_prec %>% 
                  rename(data_prec = data), by = c("lon", "lat")) %>% 
      mutate(data = purrr::map2(data, data_prec, ~left_join(.x, .y, by = "time"))) %>% 
      dplyr::select(-data_prec) %>%
      
      ## merge snow
      left_join(df_snow %>% 
                  rename(data_snow = data), by = c("lon", "lat")) %>% 
      mutate(data = purrr::map2(data, data_snow, ~left_join(.x, .y, by = "time"))) %>% 
      dplyr::select(-data_snow) %>% 
      
      mutate(data = purrr::map(data, ~dplyr::select(., time, temp, prec, snow))) %>% 
      
      ## simulate snow
      mutate(data = purrr::map(data, ~drop_na(., time))) %>% 
      mutate(data = purrr::map(data, ~simulate_snow(.)))
    
    rm("df_temp")
    rm("df_prec")
    rm("df_prec")
    
    save(df, file = path) 
    
  }
  
  error = 0
  return(error)
}