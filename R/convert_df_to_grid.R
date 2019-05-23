convert_df_to_grid <- function(df){

  ## Expand to all gridcells
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
    setNames(c("lon", "lat")) %>%
    select(-idx) %>%
    left_join(select(df, -idx), by = c("ilon", "ilat", "lon", "lat"))

  ## convert to array
  df <- df %>%
    select(lon, lat, data) %>%
    tidyr::spread(lon, data)

}