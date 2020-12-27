calc_cwd_et0_byilon <- function(ilon){
  
  source("R/calc_cwd_lue0.R")

  find_lat_lores <- function(lat_hires, vec_lat_lores){
    vec_lat_lores[which.min(abs(lat_hires - vec_lat_lores))]
  }
  
  calc_fet <- function(df){
    df %>% 
      mutate(fet = remove_outliers(et / NR, coef = 1.5)) %>% 
      rowwise() %>% 
      mutate(fet = ifelse(fet < 0.001, NA, fet)) %>% 
      mutate(fet = ifelse(is.na(NR), NA, fet))
  }
  
  convert_et_MJ <- function(x){ x * 1e6 / (24 * 60 * 60) }  # MJ m-2 d-1 -> W m-2
  
  ## construct output file name
  dirn <- "~/mct/data/df_cwd_et0/"
  filn <- paste0("df_cwd_et0_", ilon, ".RData")
  if (!dir.exists(dirn)) system("mkdir -p ~/mct/data/df_cwd_et0")
  path <- paste0(dirn, filn)
  
  if (!file.exists(path)){
    
    ## Open file CWDX output
    dirn <- "~/mct/data/df_cwdx/"
    filn <- paste0("df_cwdx_ilon_", ilon, ".RData")
    load(paste0(dirn, filn)) # loads 'df'
    
    ## extract data from CWDX output. This now contains the CWD and instances information
    df_cwd <- df %>%
      mutate(mct = purrr::map(out_mct, "mct")) %>% 
      dplyr::select(-out_mct) %>% 
      mutate(data_cwd = purrr::map(mct, "df"),
             data_inst = purrr::map(mct, "inst")) %>% 
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) %>% 
      dplyr::select(-mct)
    
    ## Load net radiation data (daytime net radiation in W m-2)
    filn <- paste0("GLASS07B01.V41._ilon_", ilon, ".RData")
    dirn <- "~/data/glass/data_tidy/"
    load(paste0(dirn, filn)) # loads 'df'
    df_netrad <- df %>% 
      rename(data_netrad = data) %>% 
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) 
    rm("df")
    
    ## Load ALEXI ET data (in MJ d-1 m-2)
    filn <- paste0("EDAY_CERES__ilon_", ilon, ".RData")
    dirn <- "~/data/alexi_tir/data_tidy/"
    load(paste0(dirn, filn)) # loads 'df'
    
    df <- df %>% 
      
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) %>% 
      rename(data_alexi = data) %>% 
      
      ## convert ALEXI ET data to W m-2 (mean across entire day; problem for comparison to netrad if significant condensation at night)
      dplyr::mutate(data_alexi = purrr::map(data_alexi, ~mutate(., et = convert_et_MJ(et)))) %>% 
      
      ## Combine with netrad data
      inner_join(mutate(df_netrad, lon = round(lon, digits = 3), lat = round(lat, digits = 3)), by = c("lon", "lat")) %>% 
      mutate(data_alexi = purrr::map2(data_alexi, data_netrad, ~left_join(.x, .y, by = "time"))) %>% 
      dplyr::select(lon, lat, data = data_alexi) %>% 
    
      ## Combine with CWD data
      inner_join(mutate(df_cwd, lon = round(lon, digits = 3), lat = round(lat, digits = 3)), by = c("lon", "lat")) %>%
      
      ## filter out pixels where cwd data is missing
      mutate(notavl_cwd = purrr::map_lgl(data_cwd, ~is.null(.))) %>% 
      dplyr::filter(!notavl_cwd) %>% 
      
      ## merge data frames
      mutate(data = purrr::map2(data_cwd, data, ~left_join(.x, .y, by = "time"))) %>% 
      dplyr::select(lon, lat, data, data_inst) %>% 
      
      ## xxx try
      # filter(lat < 40.0 & lat > 25.00) %>%
      # filter(lat == 37.025) %>%

      ## get CWD at ET = 0
      mutate(out_lue0_et = purrr::map2(data, data_inst, ~calc_cwd_lue0(.x, .y, nam_lue = "et", do_plot = TRUE))) %>% 
      mutate(cwd_lue0_et = purrr::map_dbl(out_lue0_et, "cwd_lue0")) %>%
      mutate(slope_lue_et = purrr::map_dbl(out_lue0_et, "slope_lue")) %>%
      # mutate(gg_et = purrr::map(out_lue0_et, "gg")) %>%
      mutate(flue_et = purrr::map_dbl(out_lue0_et, "flue")) %>%
      mutate(cwdmax = purrr::map_dbl(out_lue0_et, "cwdmax")) %>%
      mutate(lue_cwd0_et = purrr::map_dbl(out_lue0_et, "lue_cwd0")) %>%
      mutate(lambda_decay_et = purrr::map_dbl(out_lue0_et, "lambda_decay")) %>%
      mutate(s0_teuling_et = purrr::map_dbl(out_lue0_et, "s0_teuling")) %>%
      mutate(df_flue_et = purrr::map(out_lue0_et, "df_flue")) %>%
      dplyr::select(-out_lue0_et) %>%

      ## calculate "evaporative fraction", remove outliers and points where et is zero
      mutate(data = purrr::map(data, ~calc_fet(.))) %>% 

      ## get CWD at fET = 0 (fET = ET/Rn)
      mutate(out_lue0_fet = purrr::map2(data, data_inst, ~calc_cwd_lue0(.x, .y, nam_lue = "fet", do_plot = TRUE))) %>% 
      mutate(cwd_lue0_fet = purrr::map_dbl(out_lue0_fet, "cwd_lue0")) %>%
      mutate(slope_lue_fet = purrr::map_dbl(out_lue0_fet, "slope_lue")) %>%
      # mutate(gg_fet = purrr::map(out_lue0_fet, "gg")) %>%
      mutate(flue_fet = purrr::map_dbl(out_lue0_fet, "flue")) %>%
      mutate(df_flue_fet = purrr::map(out_lue0_fet, "df_flue")) %>%
      dplyr::select(-out_lue0_fet) %>%
      
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