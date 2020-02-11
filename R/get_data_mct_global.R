#' Convert NetCDF contents to a data frame.
#'
#' Converts the output of \code{"read_nc_onefile()"} into a data frame (tibble)
#'
#' @param out An object returned by the function \link{read_nc_onefile}.
#' @return A tidy data frame with a row for each gridcell. Contains columns
#' \code{"lon"} (longitude), \code{"lat"} (latitude), and \code{"data"}
#' (containing all time steps' data for each gridcell.)
#' @export
#'
get_data_mct_global <- function(df,
                                    dir_et, fil_et_pattern, 
                                    dir_prec, fil_prec_pattern, varnam_prec, 
                                    dir_snow, fil_snow_pattern = NA, varnam_snow = NA,
                                    dir_temp, fil_temp_pattern = NA, varnam_temp = NA){


  nchunk <- 1   # xxx test: change this back to 1000 (number of chunks)
  nrows_chunk <- ceiling(nrow(df)/nchunk)
  irow <- seq(1:nrow(df))
  irow_chunk <- split(irow, ceiling(seq_along(irow)/nrows_chunk))
  
  df <- purrr::map_dfr(
    as.list(1:length(irow_chunk)),
    ~get_data_mct_chunk( slice(ungroup(df), irow_chunk[[.]]), ., 
                             dir_et, fil_et_pattern, 
                             dir_prec, fil_prec_pattern, varnam_prec, 
                             dir_snow, fil_snow_pattern, varnam_snow,
                             dir_temp, fil_temp_pattern, varnam_temp)
    )
    
  return(df)
}

get_data_mct_chunk <- function(df, idx, 
                                   dir_et, fil_et_pattern, 
                                   dir_prec, fil_prec_pattern, varnam_prec, 
                                   dir_snow, fil_snow_pattern = NA, varnam_snow = NA,
                                   dir_temp, fil_temp_pattern, varnam_temp){
  
  print("----------------")
  print(paste("DOIN IT BY CHUNK", as.character(idx)))
  print("----------------")
  
  ##-------------------------------------------------
  ## Read all points' climate data from this chunk at once
  ##-------------------------------------------------

  ##-------------------------------------------------
  ## Precipitation
  ##-------------------------------------------------
  print("getting precipitation data from WATCH-WFDEI ...")
  convert_prec_watch <- function(x){ x * 60 * 60 * 24 }  # kg/m2/s -> mm/day
  list_fil_prec <- list.files(dir_prec, pattern = fil_prec_pattern)

  ## xxx debug: get data for one year only. 2007: [337:348]
  df_prec <- extract_points_filelist(df, list_fil_prec, varnam = "Rainf", dirnam = dir_prec, fil_pattern = fil_prec_pattern) %>%
    dplyr::rename(df_prec = data0) %>%
    dplyr::mutate(df_prec = purrr::map(df_prec, ~rename(., prec = V1))) %>%
    dplyr::mutate(df_prec = purrr::map(df_prec, ~mutate(., prec = convert_prec_watch(prec)))) %>%
    dplyr::mutate(df_prec = purrr::map(df_prec, ~drop_na(.)))

  ##-------------------------------------------------
  ## Snow
  ##-------------------------------------------------
  print("getting snow data from WATCH-WFDEI ...")
  list_fil_snow <- list.files(dir_snow, pattern = fil_snow_pattern)

  df_snow <- extract_points_filelist(df, list_fil_snow, varnam = "Snowf", dirnam = dir_snow, fil_pattern = fil_snow_pattern) %>%
    dplyr::rename(df_snow = data0) %>%
    dplyr::mutate(df_snow = purrr::map(df_snow, ~rename(., snow = V1))) %>%
    dplyr::mutate(df_snow = purrr::map(df_snow, ~mutate(., snow = convert_prec_watch(snow)))) %>%
    dplyr::mutate(df_snow = purrr::map(df_snow, ~drop_na(.)))

  ##-------------------------------------------------
  ## Temperature
  ##-------------------------------------------------
  print("getting temperature data from WATCH-WFDEI ...")
  convert_temp_watch <- function(x){ x - 273.15 }  # K -> degC
  list_fil_temp <- list.files(dir_temp, pattern = fil_temp_pattern)

  df_temp <- extract_points_filelist(df, list_fil_temp, varnam = "Tair", dirnam = dir_temp, fil_pattern = fil_temp_pattern) %>%
    dplyr::rename(df_temp = data0) %>%
    dplyr::mutate(df_temp = purrr::map(df_temp, ~rename(., temp = V1))) %>%
    dplyr::mutate(df_temp = purrr::map(df_temp, ~mutate(., temp = convert_temp_watch(temp)))) %>%
    dplyr::mutate(df_temp = purrr::map(df_temp, ~drop_na(.)))

  ##-------------------------------------------------
  ## Merge prec, snow, and temp
  ##-------------------------------------------------
  df <- df_temp %>%
    dplyr::left_join(df_prec, by = c("lon", "lat", "idx", "elv")) %>%
    dplyr::left_join(df_snow, by = c("lon", "lat", "idx", "elv")) %>%
    mutate(df_watch = purrr::map2(df_temp, df_prec, ~left_join(.x, .y, by = "date"))) %>%
    select(-df_prec, -df_temp) %>%
    mutate(df_watch = purrr::map2(df_watch, df_snow, ~left_join(.x, .y, by = "date"))) %>%
    select(-df_snow)
  
  ##-------------------------------------------------
  ## ET from LandFlux data (given in W m-2), components individually
  ##-------------------------------------------------
  print("getting ET data from LandFlux ...")
  list_fil_et <- list.files(dir_et, pattern = fil_et_pattern)
  convert_et_wm2 <- function(x){ x * 60 * 60 * 24 }  # W m-2 -> J m-2 d-1
  
  ## transpiration ("ET_tran")
  df <- extract_points_filelist(df, list_fil_et, varnam = "ET_tran", dirnam = dir_et, fil_pattern = fil_et_pattern, filetype = "landflux") %>% 
    dplyr::rename(df_et = data0) %>% 
    dplyr::mutate(df_et = purrr::map(df_et, ~rename(., transp = V1))) %>% 
    dplyr::mutate(df_et = purrr::map(df_et, ~mutate(., transp = convert_et_wm2(transp))))

  ## soil evaportation ("ET_soil")
  df <- df %>% 
    extract_points_filelist(list_fil_et, varnam = "ET_soil", dirnam = dir_et, fil_pattern = fil_et_pattern, filetype = "landflux") %>% 
    dplyr::mutate(data0 = purrr::map(data0, ~rename(., evap_soil = V1))) %>% 
    dplyr::mutate(df_et = purrr::map2(df_et, data0, ~left_join(.x, .y, by = "date"))) %>% 
    dplyr::mutate(df_et = purrr::map(df_et, ~mutate(., evap_soil = convert_et_wm2(evap_soil)))) %>% 
    dplyr::select(-data0)

  ## (wet) canopy evaporation ("can_evap")
  df <- df %>% 
    extract_points_filelist(list_fil_et, varnam = "can_evap", dirnam = dir_et, fil_pattern = fil_et_pattern, filetype = "landflux") %>%
    dplyr::mutate(data0 = purrr::map(data0, ~dplyr::rename(., evap_canop = V1))) %>% 
    dplyr::mutate(df_et = purrr::map2(df_et, data0, ~left_join(.x, .y, by = "date"))) %>% 
    dplyr::mutate(df_et = purrr::map(df_et, ~mutate(., evap_canop = convert_et_wm2(evap_canop)))) %>% 
    dplyr::select(-data0)

  ## get total ET
  df <- df %>% 
    dplyr::mutate(df_et = purrr::map(df_et, ~mutate(., et = transp + evap_soil + evap_canop)))


  ##-------------------------------------------------
  ## Merge ET and WATCH data
  ##-------------------------------------------------
  df <- df %>% 
    mutate(df = purrr::map2(df_et, df_watch, ~full_join(.x, .y, by = "date"))) %>%
    select(-df_et, -df_watch) 

  ##-------------------------------------------------
  ## Convert ET units to mm d-1
  ##-------------------------------------------------
  print("converting ET units to mm d-1 ...")
  row_rep <- function(df, n) {
    df[rep(1:nrow(df), times = n),]
  }

  df <- df %>% 

    ## add elevation to the nested dataframes (repeating same value for each time step)
    nest(df_elv = elv) %>%
    mutate(df_elv = purrr::map2(df_elv, df, ~row_rep(.x, nrow(.y)))) %>%
    mutate(df     = purrr::map2(df, df_elv, ~bind_cols(.x, .y))) %>%
    select(-df_elv) %>% 

    ## convert units: get ET in mm d-1
    ## transpiration
    mutate(transp_mm = purrr::map(df, ~convert_et(.$transp, .$temp, .$elv))) %>% 
    mutate(transp_mm = purrr::map(transp_mm, ~tibble(transp_mm = .))) %>% 
    mutate(df        = purrr::map2(df, transp_mm, ~bind_cols(.x, .y))) %>% 
    select(-transp_mm) %>% 

    ## soil evaporation
    mutate(evap_soil_mm = purrr::map(df, ~convert_et(.$evap_soil, .$temp, .$elv))) %>% 
    mutate(evap_soil_mm = purrr::map(evap_soil_mm, ~tibble(evap_soil_mm = .))) %>% 
    mutate(df        = purrr::map2(df, evap_soil_mm, ~bind_cols(.x, .y))) %>% 
    select(-evap_soil_mm) %>% 

    ## (wet) canopy evaporation
    mutate(evap_canop_mm = purrr::map(df, ~convert_et(.$evap_canop, .$temp, .$elv))) %>% 
    mutate(evap_canop_mm = purrr::map(evap_canop_mm, ~tibble(evap_canop_mm = .))) %>% 
    mutate(df        = purrr::map2(df, evap_canop_mm, ~bind_cols(.x, .y))) %>% 
    select(-evap_canop_mm) %>% 

    ## total ET
    mutate(et_mm = purrr::map(df, ~convert_et(.$et, .$temp, .$elv))) %>% 
    mutate(et_mm = purrr::map(et_mm, ~tibble(et_mm = .))) %>% 
    mutate(df        = purrr::map2(df, et_mm, ~bind_cols(.x, .y))) %>% 
    select(-et_mm)
    
  # df <- df %>% 
  #   mutate(data = purrr::map(out_ilon_ilat, ~get_data_mct_gridcell( .$ilon, .$ilat, ...)))
  
  # outfil <- paste0("./data/v3/df_plantwhc_mct", as.character(idx), ".Rdata")
  # print(paste("Saving to", outfil, "..."))
  # save(df, file = outfil)
  # print("... done.")
  # save(idx, file = "./data/idx.Rdata")
  # dir <- "~/sofun/output_nc_global_sofun/"
  # gridfile <- "./data/df_grid.Rdata"
  # load(gridfile)
  # load <- "./data/idx.Rdata"
  
  return(df)
}
