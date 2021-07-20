regrid_df <- function(df, varnam, dlon, dlat){
  
  ## bin to half-degree gridcells for determining climate forcing
  # dlon <- 0.1
  # dlat <- 0.1
  lon_breaks <- seq(from = floor(min(df$lon)), to = ceiling(max(df$lon)), by = dlon)
  lat_breaks <- seq(from = floor(min(df$lat)), to = ceiling(max(df$lat)), by = dlat)
  
  df <- df %>%
    ungroup() %>%
    mutate(ilon = cut(lon,
                      breaks = lon_breaks),
           ilat = cut(lat,
                      breaks = lat_breaks)) %>%
    mutate(lon_lower = as.numeric( sub("\\((.+),.*", "\\1", ilon)),
           lon_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilon) ),
           lat_lower = as.numeric( sub("\\((.+),.*", "\\1", ilat) ),
           lat_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilat) )) %>%
    mutate(lon_mid = (lon_lower + lon_upper)/2,
           lat_mid = (lat_lower + lat_upper)/2) %>%
    
    ## create cell name to associate with climate input
    dplyr::select(-ilon, -ilat, -lon_lower, -lon_upper, -lat_lower, -lat_upper)
  
  df <- df %>% 
    group_by(lon_mid, lat_mid) %>% 
    summarise(!!varnam := mean(!!varnam, na.rm = TRUE)) %>% 
    rename(lon = lon_mid, lat = lat_mid)
    # mutate(!!varnam := ifelse(is.nan(!!varnam), NA, !!varnam))
  
  return(df)
}