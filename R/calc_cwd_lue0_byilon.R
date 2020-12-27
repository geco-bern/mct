calc_cwd_lue0_byilon <- function(ilon){
  
  source("R/calc_cwd_lue0.R")

  find_lat_lores <- function(lat_hires, vec_lat_lores){
    vec_lat_lores[which.min(abs(lat_hires - vec_lat_lores))]
  }
  
  ## construct output file name
  dirn <- "~/mct/data/df_cwd_lue0/"
  filn <- paste0("df_cwd_lue0_", ilon, ".RData")
  if (!dir.exists(dirn)) system("mkdir -p ~/mct/data/df_cwd_lue0")
  path <- paste0(dirn, filn)
  
  if (!file.exists(path)){
    
    ## Open WATCH-WFDEI SWdown data
    lon_lores <- seq(-179.75, 179.75, by = 0.5)
    lon_hires <- seq(-179.975, 179.975, by = 0.05)
    ilon_lores <- which.min(abs(lon_lores - lon_hires[ilon]))
    load(paste0("~/data/watch_wfdei/data_tidy/SWdown_daily_WFDEI__ilon_", ilon_lores, ".RData"))
    df_sw <- df %>% 
      mutate(lon = round(lon, digits = 2), lat = round(lat, digits = 2))
    rm("df")
    
    ## filter watch data to years within ALEXI data availability (2003-2017)
    df_sw <- df_sw %>% 
      ungroup() %>% 
      dplyr::filter(!is.null(data)) %>% 
      mutate(data = purrr::map(data, ~dplyr::filter(., lubridate::year(time)>2002 & lubridate::year(time)<2018))) %>% 
      mutate(data = purrr::map(data, ~rename(., sw = SWdown)))
    
    ## get closest matching latitude indices and merge data frames
    vec_lat_lores <- df_sw$lat %>% unique()
    
    ## Open file CWDX output
    dirn <- "~/mct/data/df_cwdx/"
    filn <- paste0("df_cwdx_ilon_", ilon, ".RData")
    load(paste0(dirn, filn)) # loads 'df'
    
    ## extract data from CWDX output. This now contains the CWD and instances information
    df_cwd <- df %>% 
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) %>%
      mutate(mct = purrr::map(out_mct, "mct")) %>% 
      dplyr::select(-out_mct) %>% 
      mutate(data_cwd = purrr::map(mct, "df"),
             data_inst = purrr::map(mct, "inst")) %>% 
      dplyr::select(-mct)
    
    ## Load SiF data
    ## version PK
    filn <- paste0("GOME_PK_dcSIF_005deg_8day__ilon_", ilon, ".RData")
    dirn <- "~/data/gome_2_sif_downscaled/data_tidy/"
    load(paste0(dirn, filn)) # loads 'df'
    df_pk <- df %>% 
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) %>%
      rename(data_pk = data)
    
    ## version JJ
    filn <- paste0("GOME_JJ_dcSIF_005deg_8day__ilon_", ilon, ".RData")
    dirn <- "~/data/gome_2_sif_downscaled/data_tidy/"
    load(paste0(dirn, filn)) # loads 'df'
    
    df <- df %>% 
      
      ## because of numerical imprecision
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) %>%
      
      ## Take mean of two versions of SIF data
      rename(data_jj = data) %>% 
      full_join(df_pk, by = c("lon", "lat")) %>% 
      mutate(data = purrr::map2(data_jj, data_pk, ~bind_rows(.x, .y))) %>% 
      dplyr::select(-data_jj, -data_pk) %>% 
      mutate(data = purrr::map(data, ~group_by(., time) %>% summarise(SIF = mean(SIF, na.rm = TRUE), .groups = "drop"))) %>% 
      
      ## Combine with CWD data
      inner_join(df_cwd, by = c("lon", "lat")) %>% 
      
      ## filter missing
      dplyr::filter(!map_lgl(data, is.null)) %>% 
      dplyr::filter(!map_lgl(data_cwd, is.null)) %>% 
      dplyr::filter(!map_lgl(data_inst, is.null)) %>% 
      
      mutate(data = purrr::map2(data_cwd, data, ~left_join(.x, .y, by = "time"))) %>% 
      dplyr::select(lon, lat, data, data_inst) %>% 
      
      ## Combine with SWdown data from WATCH-WFDEI (0.5 deg!)
      mutate(lat_lores = purrr::map_dbl(lat, ~find_lat_lores(., vec_lat_lores = vec_lat_lores))) %>% 
      left_join(df_sw %>% 
                  rename(lon_lores = lon, lat_lores = lat, data_sw = data),
                by = "lat_lores") %>% 
      mutate(data = purrr::map2(data, data_sw, ~right_join(.x, .y, by = "time"))) %>% 
      dplyr::select(-data_sw) %>% 
      
      ## xxx try
      # filter(lat < 40.0 & lat > 25.00) %>%
      # filter(lat == 37.025) %>%
      
      ## get CWD at SIF = 0, using function calc_cwd_lue0()
      mutate(out_lue0_SIF = purrr::map2(data, data_inst, ~calc_cwd_lue0(.x, .y, nam_lue = "SIF", do_plot = FALSE))) %>%
      mutate(cwd_lue0_SIF = purrr::map_dbl(out_lue0_SIF, "cwd_lue0")) %>%
      mutate(slope_lue_SIF = purrr::map_dbl(out_lue0_SIF, "slope_lue")) %>%
      # mutate(gg_SIF = purrr::map(out_lue0_SIF, "gg")) %>%
      mutate(flue_SIF = purrr::map_dbl(out_lue0_SIF, "flue")) %>%
      mutate(cwdmax = purrr::map_dbl(out_lue0_SIF, "cwdmax")) %>%
      mutate(lambda_decay_SIF = purrr::map_dbl(out_lue0_SIF, "lambda_decay")) %>%
      mutate(df_flue_SIF = purrr::map(out_lue0_SIF, "df_flue")) %>%
      dplyr::select(-out_lue0_SIF) %>%
      
      ## get CWD at SIF/SWdown = 0; SIF/SWdown := nSIF
      mutate(data = purrr::map(data, ~mutate(., nSIF = SIF / sw))) %>% 
      mutate(out_lue0_nSIF = purrr::map2(data, data_inst, ~calc_cwd_lue0(.x, .y, nam_lue = "nSIF", do_plot = FALSE))) %>% 
      mutate(cwd_lue0_nSIF = purrr::map_dbl(out_lue0_nSIF, "cwd_lue0")) %>%
      mutate(slope_lue_nSIF = purrr::map_dbl(out_lue0_nSIF, "slope_lue")) %>%
      # mutate(gg_nSIF = purrr::map(out_lue0_nSIF, "gg")) %>%
      mutate(flue_nSIF = purrr::map_dbl(out_lue0_nSIF, "flue")) %>%
      mutate(lambda_decay_nSIF = purrr::map_dbl(out_lue0_nSIF, "lambda_decay")) %>%
      mutate(df_flue_nSIF = purrr::map(out_lue0_nSIF, "df_flue")) %>%
      dplyr::select(-out_lue0_nSIF) %>%
      
      ## drop data again
      dplyr::select(-data, -data_inst)
    
    ## write to file
    print(paste("Writing file:", path))
    save(df, file = path)  
    
  } else {
    print(paste("File exists already:", path))
  } 
  error = 0
  return(0)
}