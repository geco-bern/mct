extract_cwdx_byilon_lores <- function(ilon, overwrite = FALSE){
  
  infil <- paste0("data/df_cwdx_lores/df_cwdx_ilon_", ilon, ".RData")
  
  df <- NA
  
  if (file.exists(infil)){
    
    load(infil)
    
    dirn <- "~/mct/data/df_cwdx_10_20_40_lores/"
    filn <- paste0("df_cwdx_10_20_40_ilon_", ilon, ".RData")
    if (!dir.exists(dirn)) system("mkdir -p ~/mct/data/df_cwdx_10_20_40_lores")
    path <- paste0(dirn, filn)
    
    if (!file.exists(path) || overwrite){
      
      df <- df %>%
        dplyr::mutate(df_return = purrr::map(out_mct, "df_return")) %>%
        dplyr::mutate(cwdx10 = purrr::map_dbl(df_return, ~dplyr::filter(., return_period==10) %>% dplyr::pull(return_level)),
                      cwdx20 = purrr::map_dbl(df_return, ~dplyr::filter(., return_period==20) %>% dplyr::pull(return_level)),
                      cwdx40 = purrr::map_dbl(df_return, ~dplyr::filter(., return_period==40) %>% dplyr::pull(return_level)),
                      cwdx80 = purrr::map_dbl(df_return, ~dplyr::filter(., return_period==80) %>% dplyr::pull(return_level)),
                      cwdx100 = purrr::map_dbl(df_return, ~dplyr::filter(., return_period==100) %>% dplyr::pull(return_level)),
                      cwdx200 = purrr::map_dbl(df_return, ~dplyr::filter(., return_period==200) %>% dplyr::pull(return_level)),
                      cwdmax = purrr::map(out_mct, "mct")
        ) %>%
        mutate(cwdmax = purrr::map(cwdmax, "inst")) %>% 
        dplyr::filter(!is.na(cwdx80)) %>% 
        mutate(cwdmax = purrr::map(cwdmax, ~pull(., deficit))) %>% 
        mutate(cwdmax = purrr::map_dbl(cwdmax, ~max(.))) %>% 
        dplyr::select(lon, lat, cwdx10, cwdx20, cwdx40, cwdx80, cwdx100, cwdx200, cwdmax)
      
      print(paste("Writing file:", path))
      save(df, file = path)
      
    } else {
      print(paste("File exists already:", path))
      load(path)
    }
    
  } else {
    
    print(paste("File missing:", infil))
    
  }
  return(df)
}