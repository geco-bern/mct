convert_df_mct_to_grid <- function(df, varnam, maskfiln){

  ## Expand to all gridcells
  # maskfiln <- paste0(dir, "s1_fapar3g_v3_global.fland.nc")
  
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
  
  arr <- expand.grid(out$lon, out$lat) %>%
    as.matrix() %>%
    as_tibble() %>%
    setNames(c("lon", "lat")) %>%
    left_join(select(df, lon, lat, eval(varnam)), by = c("lon", "lat")) %>%

    ## convert to array
    select(lon, lat, eval(varnam)) %>%
    tidyr::spread(lon, eval(varnam)) %>%
    select(-lat) %>%
    as.matrix()

  return(arr)
}