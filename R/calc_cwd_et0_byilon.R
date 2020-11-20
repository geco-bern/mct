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
      dplyr::select(-mct)
    
    ## Load net radiation data (daytime net radiation in W m-2)
    filn <- paste0("GLASS07B01.V41._ilon_", ilon, ".RData")
    dirn <- "~/data/glass/data_tidy/"
    load(paste0(dirn, filn)) # loads 'df'
    df_netrad <- df %>% 
      rename(data_netrad = data)
    rm("df")
    
    ## Load ALEXI ET data (in MJ d-1 m-2)
    filn <- paste0("EDAY_CERES__ilon_", ilon, ".RData")
    dirn <- "~/data/alexi_tir/data_tidy/"
    load(paste0(dirn, filn)) # loads 'df'
    
    df <- df %>% 
      
      rename(data_alexi = data) %>% 
      
      ## convert ALEXI ET data to W m-2 (mean across entire day; problem for comparison to netrad if significant condensation at night)
      dplyr::mutate(data_alexi = purrr::map(data_alexi, ~mutate(., et = convert_et_MJ(et)))) %>% 
      
      ## Combine with netrad data
      inner_join(df_netrad, by = c("lon", "lat")) %>% 
      mutate(data_alexi = purrr::map2(data_alexi, data_netrad, ~left_join(.x, .y, by = "time"))) %>% 
      dplyr::select(lon, lat, data = data_alexi) %>% 
    
      ## Combine with CWD data
      inner_join(df_cwd, by = c("lon", "lat")) %>% 
      mutate(data = purrr::map2(data_cwd, data, ~left_join(.x, .y, by = "time"))) %>% 
      dplyr::select(lon, lat, data, data_inst) %>% 
      
      ## calculate "evaporative fraction", remove outliers and points where et is zero
      mutate(data = purrr::map(data, ~calc_fet(.))) %>% 

      ## get CWD at fET = 0 (fET = ET/Rn)
      mutate(out_lue0_fet = purrr::map2(data, data_inst, ~calc_cwd_lue0(.x, .y, nam_lue = "fet", do_plot = TRUE))) %>% 
      mutate(cwd_lue0_fet = purrr::map_dbl(out_lue0_fet, "lue0")) %>%
      mutate(cwd_lue0_exp_fet = purrr::map_dbl(out_lue0_fet, "lue0_exp")) %>%
      mutate(gg_fet = purrr::map(out_lue0_fet, "gg")) %>%
      mutate(flue_fet = purrr::map_dbl(out_lue0_fet, "flue")) %>%
      mutate(cwdmax_fet = purrr::map_dbl(out_lue0_fet, "cwdmax")) %>%
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