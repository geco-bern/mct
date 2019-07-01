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
get_plantwhc_mct_global <- function(df, dir){

  # ## preprocess data: DOES NOT MAKE SENSE!!!
  # ## 1. Convert arrays (lon-lat-time) from annual output files to 
  # ##    nested data frames and save as Rdata files.
  # #filn <- paste0(dir, "s1_fapar3g_v3_global.", as.character(year), ".d.wbal.nc")
  # list_filn <- list.files(path = dir, pattern = "s1_fapar3g_v4_global.*.d.wbal.nc")
  # list_nc <- purrr::map(list_filn, ~read_nc_onefile(paste0(dir, .)))
  # list_df <- purrr::map(list_nc, ~nc_to_df(., dropna = TRUE, filn = "./test.Rdata"))

  nchunk <- 1000
  nrows_chunk <- ceiling(nrow(df)/nchunk)
  irow <- seq(1:nrow(df))
  irow_chunk <- split(irow, ceiling(seq_along(irow)/nrows_chunk))
  
  df <- purrr::map_dfr(as.list(1:length(irow_chunk)), ~get_plantwhc_mct_chunk( slice(df, irow_chunk[[.]]), dir, . ))
    
  return(df)
}

get_plantwhc_mct_chunk <- function(df, dir, idx){
  
  print("----------------")
  print(paste("DOIN IT BY CHUNK", as.character(idx)))
  print("----------------")
  
  df <- df %>% 
    mutate(data = purrr::map(out_ilon_ilat, ~get_plantwhc_mct_gridcell( .$ilon, .$ilat, dir)))
  
  # outfil <- "./data/df_plantwhc_mct.Rdata"
  # if (file.exists(outfil)){
  #   load(outfil)
  # } else {
  #   df2 <- df
  # }
  # df2 <- df2 %>% bind_rows(df)
  # print(paste("Saving to", outfil, "..."))
  # save(df2, file = outfil)
  # print("... done.")
  
  outfil <- paste0("./data/v2/df_plantwhc_mct", as.character(idx), ".Rdata")
  print(paste("Saving to", outfil, "..."))
  save(df, file = outfil)
  print("... done.")
  save(idx, file = "./data/idx.Rdata")
  #rm(list = ls())
  dir <- "~/sofun/output_nc_global_sofun/"
  gridfile <- "./data/df_grid.Rdata"
  load(gridfile)
  load <- "./data/idx.Rdata"
  
  return(df)
}

get_plantwhc_mct_gridcell <- function(ilon, ilat, dir){
  
  print(paste("doin it by gridcell:", as.character(ilon), as.character(ilat)))
  
  ## read daily climate data (prec, PET, fAPAR) for this gridcell
  print("reading nc file...")
  ddf <- withTimeout(read_nc_gridcell(ilon, ilat, dir), timeout = 60, onTimeout = "warning")
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
    if (sum(!is.na(ddf$evi))>1){
      
      ddf$fapar <- approx( ddf$evi, xout = 1:length(ddf$evi) )$y
      
      ddf <- ddf %>% 
        # mutate(fapar = myapprox(evi)) %>%
        cutna_headtail_df("fapar", extend = TRUE) %>% 
        
        ## calculate daily water balance
        rowwise() %>% 
        mutate(wbal = water_to_soil - fapar * pet)

    } else {
      
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

get_df_landmask <- function(dir){
  
  ## get land mask (data frame with lon, lat, ilon, and ilat for each land cell)
  maskfiln <- paste0(dir, "s1_fapar3g_v3_global.fland.nc")
  
  nc <- ncdf4::nc_open(maskfiln)
  {
    sink(paste0(maskfiln, ".txt"))
    print(nc)
    sink()
    unlink(paste0(maskfiln, ".txt"))
  }

  out <- list(
    lon = ncdf4::ncvar_get(nc, nc$dim$lon$name),
    lat = ncdf4::ncvar_get(nc, nc$dim$lat$name)
  )
  
  df <- expand.grid(out$lon, out$lat) %>%
    as.matrix() %>%
    as_tibble() %>%
    setNames(c("lon", "lat"))
  
  fland <- ncdf4::ncvar_get(nc, "fland")
  ncdf4::nc_close(nc)
  
  df <- df %>%
    bind_cols(tibble(fland = as.vector(fland))) %>%
    tidyr::drop_na() %>% 
    mutate(idx = 1:n()) %>% 
    group_by(idx) %>% 
    nest() %>% 
    mutate(out_ilon_ilat = purrr::map(data, ~get_ilon_ilat( .$lon, .$lat, out$lon, out$lat )))

  return(df)  
}


read_nc_gridcell <- function(ilon, ilat, dir){
  
  ##-------------------------------------------------
  ## WBAL: in SOFUN output it's liquid-water-to-soil 
  ##-------------------------------------------------
  filn <- paste0(dir, "s1_fapar3g_v4_global.d.wbal.nc")

  nc <- ncdf4::nc_open(filn)
  # Save the print(nc) dump to a text file
  {
    sink(paste0(filn, ".txt"))
    print(nc)
    sink()
    unlink(paste0(filn, ".txt"))
  }
  
  time <- ncdf4::ncvar_get(nc, nc$dim$time$name)
  
  ## convert to date
  if (nc$dim$time$units=="days since 2001-1-1 0:0:0"){
    date <- conv_noleap_to_ymd(time, origin = lubridate::ymd("2001-01-01"))
  }

  wbal <- ncdf4::ncvar_get(nc, "wbal", start = c(ilon, ilat, 1, 1), count = c(1,1,1,length(time)))
  ncdf4::nc_close(nc)
  
  #-------------------------------------------------
  # PET
  #-------------------------------------------------
  filn <- paste0(dir, "s1_fapar3g_v4_global.d.pet.nc")

  nc <- ncdf4::nc_open(filn)
  # Save the print(nc) dump to a text file
  {
    sink(paste0(filn, ".txt"))
    print(nc)
    sink()
    unlink(paste0(filn, ".txt"))
  }

  pet <- ncdf4::ncvar_get(nc, "pet", start = c(ilon, ilat, 1, 1), count = c(1,1,1,length(time)))
  ncdf4::nc_close(nc)

  # data frame with SOFUN outputs
  df1 <- tibble(
    date = date, 
    water_to_soil = wbal,
    pet = pet
    ) %>% 
    mutate(year = year(date), doy = yday(date), moy = month(date))
  
  ##-------------------------------------------------
  ## fAPAR as EVI (ZMAW data)
  ##-------------------------------------------------
  dir2 <- "/alphadata01/bstocker/data/modis_monthly-evi/zmaw_data/halfdeg/"
  filn <- paste0(dir2, "modis_vegetation__LPDAAC__v5__halfdegMAX_mean2000.nc")
  
  nc <- ncdf4::nc_open(filn)
  # Save the print(nc) dump to a text file
  {
    sink(paste0(filn, ".txt"))
    print(nc)
    sink()
    unlink(paste0(filn, ".txt"))
  }

  time2 <- ncdf4::ncvar_get(nc, nc$dim$time$name)
  ## convert to date
  if (nc$dim$time$units=="days since 2001-1-1 0:0:0"){
    date2 <- conv_noleap_to_ymd(time2, origin = lubridate::ymd("2001-01-01"))
  }
  
  ## override
  date2 <- seq(from = ymd("2000-01-01"), to = ymd("2015-12-01"), by = "months") + days(14)
  
  # get evi from NetCDF
  # evi <- ncdf4::ncvar_get(nc, "evi", start = c(ilon, ilat, 1), count = c(1,1,length(time2)))
  evi <- ncdf4::ncvar_get(nc, "evi", start = c(ilon, ilat, 1), count = c(1,1,12))
  
  ncdf4::nc_close(nc)
  
  df2 <- tibble(date = date2, evi = evi) %>% 
    mutate(year = year(date2), moy = month(date)) #
  
  # gg <- ggplot(df2) +
  #   geom_line(aes(x = date, y = evi)) +
  #   labs( title = paste("ilon:", as.character(ilon), " ilat:", as.character(ilat)) )
  # print(gg)
  
  ## first take mean seasonality (is stored in year 2000)
  df2_meandoy <- df2 %>%
    dplyr::filter(year == 2000) %>%
    dplyr::select(-date, -year) %>%
    dplyr::rename(evi_meandoy = evi)

  ## merge mean seasonality (evi_meandoy) into the main df
  df <- df1 %>%
    left_join(df2_meandoy, by = "moy")

  ## MEAN SEASONAL CYCLE: take evi as evi_meandoy
  df <- df %>% 
    mutate(evi = evi_meandoy)
  
  # ## ACTUAL EVI FOR POST-2000: merge actual evi into main df
  # df <- df %>%
  #   left_join(dplyr::select(df2, year, moy, evi), by = c("year", "moy")) %>% 
  #   mutate(dom = mday(date)) %>% 
  #   
  #   # fill up years before 2000 with mean seasonality (2000 has already mean seasonality)
  #   rowwise() %>% 
  #   mutate(evi = ifelse(is.na(evi), evi_meandoy, evi)) %>% 
  #   
  #   # remove again all days data except the 15th of each month
  #   mutate(evi = ifelse(dom==15, evi, NA)) %>% 
  #   dplyr::select(-dom, -evi_meandoy, -year, -moy, -doy)

  
  return(df)
}

get_ilon_ilat <- function(lon, lat, lon_vec, lat_vec){

  ## get index to read data only for this index
  ilon <- which.min(abs(lon - lon_vec))
  ilat <- which.min(abs(lat - lat_vec))
  
  df <- tibble(lon=lon, lat=lat, ilon=ilon, ilat=ilat)
  
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
