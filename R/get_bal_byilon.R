get_bal_byilon <- function(ilon_hires){
  
  source("R/get_bal.R")
  
  dirn <- "~/mct/data/df_bal/"
  filn <- paste0("df_bal_ilon_", ilon_hires, ".RData")
  if (!dir.exists(dirn)) system("mkdir -p ~/mct/data/df_bal")
  path <- paste0(dirn, filn)

  if (!file.exists(path)){
  
    ## determine closest longitude in 0.5 res files (WATCH)
    lon_lores <- seq(-179.75, 179.75, by = 0.5)
    lon_hires <- seq(-179.975, 179.975, by = 0.05)
    ilon_lores <- which.min(abs(lon_lores - lon_hires[ilon_hires]))
    
    ## Open ET-mm file
    dirn <- "~/mct/data/df_alexi_et_mm/"
    filn <- paste0("df_alexi_et_mm_ilon_", ilon_hires, ".RData")
    load(paste0(dirn, filn)) # loads 'df_alexi'
    
    ## open snow file of corresponding longitude slice
    dirn <- "~/mct/data/df_snow/"
    filn <- paste0("df_snow_ilon_", ilon_lores, ".RData")
    path <- paste0(dirn, filn)
    load(path)  # loads 'df'
    df_watch <- df # rename
    rm("df")
    
    ## get closest matching latitude indices and merge data frames
    df <- df_alexi %>% 
      
      ## merge watch data into alexi data frame
      left_join(df_watch %>% 
                  rename(lon_lores = lon, lat_lores = lat, data_watch = data),
                by = c("lon_lores", "lat_lores")) %>% 
      
      ## select only time and liquid water to soil
      mutate(data_watch = purrr::map(data_watch, ~dplyr::select(., time, liquid_to_soil))) %>% 
      
      ## remove rows where watch data is missing
      ungroup() %>% 
      dplyr::filter(!is.null(data_watch)) %>% 
      
      ## merge liquid into 'data'
      mutate(data = purrr::map2(data, data_watch, ~left_join(.x, .y, by = "time"))) %>% 
      dplyr::select(-data_watch) %>% 
      
      ## interpolate ET, get water balance, and cut NA from head and tail
      # slice(50) %>% 
      dplyr::mutate( data = purrr::map(
        data, 
        ~get_bal(., varnam_bal = "bal", varnam_prec = "liquid_to_soil", varnam_et = "et_mm"))
      ) 
    
    save(df, file = path)
    
  } else {
    print(paste("File exists already:", path))
  } 
  
  error = 0
  return(error)
}