## Reads a land mask file and creates a data frame with lon, lat, ilon, ilat for each land cell
get_df_landmask <- function(maskfiln){
  
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
  
  elv <- ncdf4::ncvar_get(nc, "elevation")
  ncdf4::nc_close(nc)
  
  df <- df %>%
    bind_cols(tibble(elv = as.vector(elv))) %>%
    tidyr::drop_na() %>% 
    mutate(idx = 1:n())
    # group_by(idx) %>% 
    # nest() %>% 
    # mutate(out_ilon_ilat = purrr::map(data, ~get_ilon_ilat( .$lon, .$lat, out$lon, out$lat )))
  
  return(df)  
}

get_ilon_ilat <- function(lon, lat, lon_vec, lat_vec){
  
  ## get index to read data only for this index
  ilon <- which.min(abs(lon - lon_vec))
  ilat <- which.min(abs(lat - lat_vec))
  
  df <- tibble(lon=lon, lat=lat, ilon=ilon, ilat=ilat)
  
  return(df)
}
