get_plantwhc_mct_global <- function(df,
                                    dir_et, fil_et_pattern, varnam_et, 
                                    dir_prec, fil_prec_pattern, varnam_prec, 
                                    dir_snow, fil_snow_pattern = NA, varnam_snow = NA,
                                    dir_temp, fil_temp_pattern = NA, varnam_temp = NA,
                                    do_slice = TRUE){

  # ## preprocess data: DOES NOT MAKE SENSE!!!
  # ## 1. Convert arrays (lon-lat-time) from annual output files to 
  # ##    nested data frames and save as Rdata files.
  # #filn <- paste0(dir, "s1_fapar3g_v3_global.", as.character(year), ".d.wbal.nc")
  # list_filn <- list.files(path = dir, pattern = "s1_fapar3g_v4_global.*.d.wbal.nc")
  # list_nc <- purrr::map(list_filn, ~read_nc_onefile(paste0(dir, .)))
  # list_df <- purrr::map(list_nc, ~nc_to_df(., dropna = TRUE, filn = "./test.Rdata"))

  if (do_slice){
    ##-------------------------------------------------
    ## Slice up grid data frame into chunks
    ##-------------------------------------------------
    nchunk <- 1   # xxx test: change this back to 1000 (number of chunks)
    nrows_chunk <- ceiling(nrow(df)/nchunk)
    irow <- seq(1:nrow(df))
    irow_chunk <- split(irow, ceiling(seq_along(irow)/nrows_chunk))
    
    df <- purrr::map_dfr(
      as.list(1:length(irow_chunk)),
      ~get_plantwhc_mct_chunk( slice(ungroup(df), irow_chunk[[.]]), ., 
                               dir_et, fil_et_pattern, varnam_et, 
                               dir_prec, fil_prec_pattern, varnam_prec, 
                               dir_snow, fil_snow_pattern, varnam_snow,
                               dir_temp, fil_temp_pattern, varnam_temp)
      )

  } else {
    ##-------------------------------------------------
    ## Pass entire data frame
    ##-------------------------------------------------
    df <- purrr::map_dfr(
      as.list(seq(nrow(df))),
      ~get_plantwhc_mct_chunk( slice(ungroup(df), .), ., 
                               dir_et, fil_et_pattern, varnam_et, 
                               dir_prec, fil_prec_pattern, varnam_prec, 
                               dir_snow, fil_snow_pattern, varnam_snow,
                               dir_temp, fil_temp_pattern, varnam_temp)
      )
  }

  return(df)
}


get_plantwhc_mct_chunk <- function(df, idx, 
                                   dir_et, fil_et_pattern, varnam_et, 
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

  ## xxx debug: get data for one year only
  df_prec <- extract_points_filelist(df, list_fil_prec[313:324], dirnam = dir_prec, fil_pattern = fil_prec_pattern) %>%
    dplyr::rename(df_prec = data0) %>%
    dplyr::mutate(df_prec = purrr::map(df_prec, ~rename(., prec = V1))) %>%
    dplyr::mutate(df_prec = purrr::map(df_prec, ~mutate(., prec = convert_prec_watch(prec)))) %>%
    dplyr::mutate(df_prec = purrr::map(df_prec, ~drop_na(.)))

  ##-------------------------------------------------
  ## Snow
  ##-------------------------------------------------
  print("getting snow data from WATCH-WFDEI ...")
  list_fil_snow <- list.files(dir_snow, pattern = fil_snow_pattern)

  df_snow <- extract_points_filelist(df, list_fil_snow[313:324], dirnam = dir_snow, fil_pattern = fil_snow_pattern) %>%
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

  df_temp <- extract_points_filelist(df, list_fil_temp[313:324], dirnam = dir_temp, fil_pattern = fil_temp_pattern) %>%
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
  ## ET (given in W m-2)
  ##-------------------------------------------------
  print("getting ET data from PT-JPL ...")
  list_fil_et <- list.files(dir_et, pattern = fil_et_pattern)
  convert_et_wm2 <- function(x){ x * 60 * 60 * 24 }  # W m-2 -> J m-2 d-1
  
  df <- extract_points_filelist(df, list_fil_et, dirnam = dir_et, fil_pattern = fil_et_pattern, filetype = "landflux") %>% 
    dplyr::rename(df_et_pt = data0) %>% 
    dplyr::mutate(df_et_pt = purrr::map(df_et_pt, ~rename(., et = V1))) %>% 
    dplyr::mutate(df_et_pt = purrr::map(df_et_pt, ~mutate(., et = convert_et_wm2(et))))
    # dplyr::mutate(df_et_pt = purrr::map(df_et_pt, ~drop_na(.)))
  
  ##-------------------------------------------------
  ## Merge ET and WATCH data
  ##-------------------------------------------------
  df <- df %>% 
    mutate(df = purrr::map2(df_et_pt, df_watch, ~full_join(.x, .y, by = "date"))) %>%
    select(-df_et_pt, -df_watch) 

  ##-------------------------------------------------
  ## Convert ET units
  ##-------------------------------------------------
  row_rep <- function(df, n) {
    df[rep(1:nrow(df), times = n),]
  }
  df <- df %>% 
    nest(df_elv = elv) %>%
    mutate(df_elv = purrr::map2(df_elv, df, ~row_rep(.x, nrow(.y)))) %>%
    mutate(df = purrr::map2(df, df_elv, ~bind_cols(.x, .y))) %>%
    select(-df_elv) %>% 
    mutate(et_mm = purrr::map(df, ~convert_et(.$et, .$temp, .$elv))) %>% 
    mutate(et_mm = purrr::map(et_mm, ~tibble(et_mm = .))) %>% 
    mutate(df = purrr::map2(df, et_mm, ~bind_cols(.x, .y))) %>% 
    select(-et_mm)
    
  # df <- df %>% 
  #   mutate(data = purrr::map(out_ilon_ilat, ~get_plantwhc_mct_gridcell( .$ilon, .$ilat, ...)))
  
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

get_plantwhc_mct_gridcell <- function(ilon, ilat, ...){
  
  print(paste("doin it by gridcell:", as.character(ilon), as.character(ilat)))
  
  ## read daily climate data (prec, PET, fAPAR) for this gridcell
  print("reading nc file...")
  ddf <- withTimeout(
    read_nc_gridcell(
      ilon, ilat, ...
      ), 
    timeout = 60, onTimeout = "warning"
    )
  if (typeof(ddf)=="character"){
    return_period <- c(2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 120, 200, 250, 300, 500, 800)
    out_plant_whc <- tibble(
      return_period = return_period, 
      return_level = rep(NA, length(return_period))
      )
    print("TIMED OUT (60 s)")
  
  } else {
    print("... done.")
    
    ## interpolate to daily values for fapar
    if (sum(!is.na(ddf$fapar))>1){
      
      # ddf$fapar <- approx( ddf$evi, xout = 1:length(ddf$evi) )$y
      
      ddf <- ddf %>% 
        
        ## interpolate fapar
        rename(fapar_readin = fapar) %>% 
        mutate(fapar = myapprox(fapar_readin)) %>%
        cutna_headtail_df("fapar", extend = TRUE) %>% 
        
        ## calculate daily water balance
        rowwise() %>% 
        mutate(wbal = water_to_soil - fapar * pet)

      # ggplot(ddf, aes(x = date)) +
      #   geom_line( aes(y = fapar) ) +
      #   geom_point( aes(y = fapar_readin), color = "red")

    } else {
      print("WARNING: No fAPAR data for this cell.")
      ddf <- ddf %>% 
        mutate(fapar = NA, wbal = NA)
      
    }
    
    ## calculate whc_mct given daily climate data
    print("get plantwhc by site ...")
    
    ##----------------------------------------------
    ## call the MCT function
    ##----------------------------------------------
    out_plantwhc_mct <- withTimeout( get_plantwhc_mct_bysite(ddf), varname_wbal = "wbal", timeout = 60, onTimeout = "warning")
    ##----------------------------------------------
    
    if (typeof(out_plantwhc_mct)=="character"){
      return_period <- c(2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 120, 200, 250, 300, 500, 800)
      out_plant_whc <- tibble(
        return_period = return_period, 
        return_level = rep(NA, length(return_period))
      )
      print("TIMED OUT (60 s)")
    } else {
      print("... done.")
    }
    
  }
  
  return(out_plantwhc_mct)
  
}


read_nc_gridcell <- function(ilon, ilat, fil_et, varnam_et, fil_prec_pattern, varnam_prec, fil_snow = NA, varnam_snow = NA){
  ##-------------------------------------------------
  ## Precipitation (accounting for snow vs. rain and 
  ## snow melt. In SOFUN output it's liquid-water-to-soil 
  ##-------------------------------------------------
  #fil_prec <- paste0(dir, "s1_fapar3g_v4_global.d.wbal.nc")
  list_fil_prec <- list.files("~/data/watch_wfdei/Rainf_daily", pattern = fil_prec_pattern)
  
  ## use raster to read point data from file
  
  
  
  nc <- ncdf4::nc_open(fil_prec)
  # Save the print(nc) dump to a text file
  {
    sink(paste0(fil_prec, ".txt"))
    print(nc)
    sink()
    unlink(paste0(fil_prec, ".txt"))
  }
  
  time <- ncdf4::ncvar_get(nc, nc$dim$time$name)
  
  ## convert to date
  if (nc$dim$time$units=="days since 2001-1-1 0:0:0"){
    date <- conv_noleap_to_ymd(time, origin = lubridate::ymd("2001-01-01"))
  }

  wbal <- ncdf4::ncvar_get(nc, varnam_prec, start = c(ilon, ilat, 1, 1), count = c(1,1,1,length(time)))
  ncdf4::nc_close(nc)
  
  #-------------------------------------------------
  # ET
  #-------------------------------------------------
  nc <- ncdf4::nc_open(fil_et)
  # Save the print(nc) dump to a text file
  {
    sink(paste0(fil_et, ".txt"))
    print(nc)
    sink()
    unlink(paste0(fil_et, ".txt"))
  }

  et <- ncdf4::ncvar_get(nc, varnam_et, start = c(ilon, ilat, 1, 1), count = c(1,1,1,length(time)))
  ncdf4::nc_close(nc)

  # data frame with SOFUN outputs
  df1 <- tibble(
    date = date, 
    water_to_soil = wbal,
    et = et
    ) %>% 
    mutate(year = year(date), doy = yday(date), moy = month(date))

  return(df)
}

as_tibble_byvar <- function(varnam, out){
  data <- out$vars[[varnam]] %>% as.vector()
  df <- tibble( varnam = data )
  return(df)
}

as_tibble_byvar_slice1 <- function(varnam, out){
  data <- out$vars[[varnam]][,,1] %>% as.vector()
  df <- tibble( varnam = data )
  return(df)
}
