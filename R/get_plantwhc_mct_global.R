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

  nchunk <- 1000
  nrows_chunk <- ceiling(nrow(df)/nchunk)
  irow <- seq(1:nrow(df))
  irow_chunk <- split(irow, ceiling(seq_along(irow)/nrows_chunk))
  
  df <- purrr::map_dfr(as.list(seq(length(irow_chunk))), ~get_plantwhc_mct_chunk( slice(df, irow_chunk[[.]]), dir, . ))
  
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
  
  outfil <- paste0("./data/df_plantwhc_mct", as.character(idx), ".Rdata")
  print(paste("Saving to", outfil, "..."))
  save(df, file = outfil)
  print("... done.")

  return(df)
}

get_plantwhc_mct_gridcell <- function(ilon, ilat, dir){
  
  print(paste("doin it by gridcell:", as.character(ilon), as.character(ilat)))
  print("reading nc file...")
  ddf <- read_nc_gridcell(ilon, ilat, dir)
  print("... done.")
  
  print("get plantwhc by site ...")
  out_plantwhc_mct <- get_plantwhc_mct_bysite(ddf)
  print("... done.")
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
  
  # years <- seq(1982, 2016)
  years <- seq(1982, 2016)
  
  df <- purrr::map_dfr(as.list(years), ~read_nc_gridcell_oneyear(., ilon, ilat, dir))
  
  return(df)
}

read_nc_gridcell_oneyear <- function(year, ilon, ilat, dir){
  
  filn <- paste0(dir, "s1_fapar3g_v3_global.", as.character(year), ".d.wbal.nc")

  nc <- ncdf4::nc_open(filn)
  # Save the print(nc) dump to a text file
  {
    sink(paste0(filn, ".txt"))
    print(nc)
    sink()
  }
  
  time <- ncdf4::ncvar_get(nc, nc$dim$time$name)
  
  ## convert to date
  if (nc$dim$time$units=="days since 2001-1-1 0:0:0"){
    date <- conv_noleap_to_ymd(time, origin = lubridate::ymd("2001-01-01"))
  }

  wbal <- ncdf4::ncvar_get(nc, "wbal", start = c(ilon, ilat, 1, 1), count = c(1,1,1,length(time)) )
  
  df <- tibble(date = date, wbal = wbal)
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
