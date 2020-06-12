simulate_snow <- function(df){

  temp <- df$temp
  prec <- df$prec
  snow <- df$snow

  ## fixed parameters
  temp_threshold <- 1.0
  maxmeltrate <- 1.0

  snow_pool <- 0
  liquid_to_soil <- rep(NA, length(prec))
  snow_pool_out <- rep(NA, length(prec))

  ## spinup 1 year
  for (doy in 1:365){
    if ( snow_pool > 0.0 && temp[doy] > temp_threshold ){
      melt  <- min( snow_pool, maxmeltrate * ( temp[doy] - temp_threshold ) )
    } else {
      melt <- 0.0
    }
    snow_pool <- snow_pool + snow[doy] - melt
  }

  ## transient forward
  for (doy in 1:length(prec)){
    if ( snow_pool > 0.0 && temp[doy] > temp_threshold ){
      melt  <- min( snow_pool, maxmeltrate * ( temp[doy] - temp_threshold ) )
    } else {
      melt <- 0.0
    }
    snow_pool <- snow_pool + snow[doy] - melt
    liquid_to_soil[doy] <- prec[doy] + melt
    snow_pool_out[doy] <- snow_pool
  }

  ## complement
  df$liquid_to_soil <- liquid_to_soil
  df$snow_pool <- snow_pool_out

  return(df)

}