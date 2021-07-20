get_cwdx_byilon_lores <- function(ilon){
  
  source("R/mct2.R")
  source("R/get_plantwhc_mct_bysite.R")
  
  ## construct output file name
  dirn <- "~/mct/data/df_cwdx_lores/"
  filn <- paste0("df_cwdx_ilon_", ilon, ".RData")
  if (!dir.exists(dirn)) system("mkdir -p ~/mct/data/df_cwdx_lores")
  path <- paste0(dirn, filn)
  
  if (!file.exists(path)){
    
    ## Open file with daily water balance
    dirn <- "~/mct/data/df_bal_lores/"
    filn <- paste0("df_bal_ilon_", ilon, ".RData")
    load(paste0(dirn, filn)) # loads 'df'
    
    ## determine CWD and events
    df <- df %>% 
      
      ## use only years 2003-2018 (in correspondence with ALEXI-ET data)
      mutate(data = purrr::map(data, ~dplyr::filter(., lubridate::year(time) %in% 2003:2018))) %>% 
      
      ## round to avoid numerical imprecision
      mutate(lon = round(lon, digits = 2), lat = round(lat, digits = 2)) %>%
      
      dplyr::mutate(
        out_mct = purrr::map(
          data,
          ~get_plantwhc_mct_bysite(
            .,
            varname_wbal = "bal",
            varname_date = "time",
            thresh_terminate = 0.0,
            thresh_drop = 0.9,
            fittype = "Gumbel"))
      ) %>% 
      dplyr::select(-data)
    
    print(paste("Writing file:", path))
    save(df, file = path)
    
  }

  error = 0
  return(error)
}