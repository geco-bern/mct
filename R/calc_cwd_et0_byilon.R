calc_cwd_et0_byilon <- function(ilon, drop_data = TRUE,
                                dirn = "data/df_cwd_et0_2/",
                                verbose = FALSE, overwrite = FALSE,
                                siteinfo = NULL, use_lat = NULL,
                                do_plot = TRUE,
                                config = read_input_config()){
  
  source("R/calc_cwd_lue0_v2.R")

  find_lat_lores <- function(lat_hires, vec_lat_lores){
    vec_lat_lores[which.min(abs(lat_hires - vec_lat_lores))]
  }
  
  calc_fet <- function(df){
    df %>% 
      mutate(fet = rbeni::remove_outliers(et / NR, coef = 1.5)) %>% 
      rowwise() %>% 
      mutate(fet = ifelse(fet < 0.001, NA, fet)) %>% 
      mutate(fet = ifelse(is.na(NR), NA, fet))
  }
  
  ## construct output file name
  path <- climate_output_path(
    file.path(dirn, paste0("df_cwd_et0_", ilon, ".RData")),
    config
  )
  ensure_directory(dirname(path))
  
  if (!file.exists(path) || overwrite){
    
    ## Open file CWDX output
    cwdx_path <- climate_output_path(
      paste0("data/df_cwdx/df_cwdx_ilon_", ilon, ".RData"),
      config
    )
    load(cwdx_path) # loads 'df'
    
    ## extract data from CWDX output. This now contains the CWD and instances information
    df_cwd <- df %>%
      mutate(mct = purrr::map(out_mct, "mct")) %>% 
      dplyr::select(-out_mct) %>% 
      mutate(data_cwd = purrr::map(mct, "df"),
             data_inst = purrr::map(mct, "inst")) %>% 
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) %>% 
      dplyr::select(-mct)
    
    ## Load net radiation data (daytime net radiation in W m-2)
    netrad_path <- climate_output_path(
      paste0(
        "~/data/glass/data_tidy/GLASS07B01.V41._ilon_",
        ilon,
        ".RData"
      ),
      config
    )
    load(path.expand(netrad_path)) # loads 'df'
    df_netrad <- df %>% 
      rename(data_netrad = data) %>% 
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) 
    rm("df")
      
    ## Load configured ET data
    et_source <- config$et$source
    load(source_tidy_path(et_source, ilon, config)) # loads 'df'
    
    ## this is for checks at certain sites
    if (!is.null(siteinfo)){
      idx_keep <- which.min(abs(siteinfo$lat - df$lat))
      df <- df[idx_keep,]
    }
    if (!is.null(use_lat)){
      idx_keep <- which.min(abs(use_lat - df$lat))
      df <- df[idx_keep,]
    }
    
    df <- df %>% 
      
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) %>% 
      rename(data_alexi = data) %>%
      mutate(data_alexi = purrr::map(
        data_alexi,
        ~dplyr::rename(.x, et = tidyselect::all_of(et_source$variable))
      )) %>%
      
      ## convert ET data to W m-2 (mean across the entire day)
      dplyr::mutate(
        data_alexi = purrr::map(data_alexi, ~mutate(.x, et = et_to_w_m2(et, et_source)))
      ) %>%
      
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
      
      # ## xxx try
      # # filter(lat < 40.0 & lat > 25.00) %>%
      # filter(lat == 44.675) %>%

      # ## get CWD at ET = 0
      # mutate(out_lue0_et = purrr::map2(data, data_inst, ~calc_cwd_lue0(.x, .y, nam_lue = "et", do_plot = FALSE))) %>% 
      # mutate(cwd_lue0_et = purrr::map_dbl(out_lue0_et, "cwd_lue0")) %>%
      # # mutate(gg_et = purrr::map(out_lue0_et, "gg")) %>%
      # mutate(flue_et = purrr::map_dbl(out_lue0_et, "flue")) %>%
      # mutate(cwdmax = purrr::map_dbl(out_lue0_et, "cwdmax")) %>%
      # mutate(lambda_decay_et = purrr::map_dbl(out_lue0_et, "lambda_decay")) %>%
      # mutate(s0_teuling_et = purrr::map_dbl(out_lue0_et, "s0_teuling")) %>%
      # dplyr::select(-out_lue0_et) %>%

      ## calculate "evaporative fraction", remove outliers and points where et is zero
      mutate(data = purrr::map(data, ~calc_fet(.))) %>% 

      ## get CWD at fET = 0 (fET = ET/Rn)
      mutate(out_lue0_fet = purrr::map2(data, data_inst, ~calc_cwd_lue0(.x, .y, nam_lue = "fet", do_plot = do_plot, verbose = verbose))) %>% 
      
      ## keep parameters of fits
      mutate(cwd_lue0_fet = purrr::map_dbl(out_lue0_fet, "cwd_lue0")) %>%
      mutate(flue_fet = purrr::map_dbl(out_lue0_fet, "flue")) %>%
      mutate(cwdmax = purrr::map_dbl(out_lue0_fet, "cwdmax")) %>%
      mutate(lambda_decay_fet = purrr::map_dbl(out_lue0_fet, "lambda_decay")) %>%
      mutate(s0_teuling_fet = purrr::map_dbl(out_lue0_fet, "s0_teuling")) %>%
      mutate(type_fet = purrr::map_chr(out_lue0_fet, "type")) %>%
      mutate(cwd_flattening_fet = purrr::map_dbl(out_lue0_fet, "cwd_flattening")) %>% 
      mutate(gg_fet = purrr::map(out_lue0_fet, "gg"))

    if (drop_data){
      ## drop data again
      df <- df %>% 
        dplyr::select(-data, -data_inst, -out_lue0_fet)
    }
    
    ## write to file
    print(paste("Writing file:", path))
    save(df, file = path)  
    
  } else {
    print(paste("File exists already:", path))
  }
  
  error = 0
  
  if (is.null(siteinfo) && is.null(use_lat)){
    out <- error
  } else {
    out <- df
  }
  
  return(out)
}
