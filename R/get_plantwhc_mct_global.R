

get_plantwhc_mct_global <- function(df){

  df <- df %>% 
    slice(1:3) %>% 
    mutate(data = purrr::map(out_ilon_ilat, ~get_plantwhc_mct_gridcell( .$ilon, .$ilat)))
  # mutate(data = purrr::map(out_ilon_ilat, ~read_nc_gridcell( .$ilon, .$ilat)))
  
  return(df)
}

get_df_landmask <- function(){
  
  ## get land mask (data frame with lon, lat, ilon, and ilat for each land cell)
  maskfiln <- "~/sofun/output_nc_global_sofun/s1_fapar3g_v3_global.fland.nc"
  
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

get_plantwhc_mct_gridcell <- function(ilon, ilat){
  
  ddf <- read_nc_gridcell(ilon, ilat)
 
  out_plantwhc_mct <- get_plantwhc_mct_bysite(ddf)
  
  return(out_plantwhc_mct)
   
}

read_nc_gridcell <- function(ilon, ilat){
  
  # years <- seq(1982, 2016)
  years <- seq(1982, 1982)
  
  df <- purrr::map_dfr(as.list(years), ~read_nc_gridcell_oneyear(., ilon, ilat))
  
  return(df)
}

read_nc_gridcell_oneyear <- function(year, ilon, ilat){
  
  filn <- paste0("~/sofun/output_nc_global_sofun/s1_fapar3g_v3_global.", as.character(year), ".d.wbal.nc")

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
get_wbal_nc <- function(filn){

  require(dplyr)
  require(tidyr)
  require(rbeni)

  ## file 
  filn <- "~/sofun/output_nc_global_sofun/s1_fapar3g_v3_global.fland.nc"

  ## read nc file
  out <- read_nc_onefile(filn)
  varnams <- out[["varnams"]]

  if (length(out$lat)>1 && length(out$lat)>1){



  } else {

    ## expand lon-lat-time grid -> produces flat data frame
    df <- expand.grid(out$lon, out$lat, out$time) %>%
      as.matrix() %>%
      as_tibble() %>%
      setNames(c("lon", "lat", "time"))

    ## get all data as columns in a data frame
    df_data <- purrr::map_dfc(as.list(varnams), ~as_tibble_byvar(., out)) %>%
      setNames(varnams)
  }

  ## combine lon, lat, time, and data columns
  df <- df %>%
    bind_cols(df_data) %>%
    tidyr::drop_na()

  ## nest time
  df <- df %>%
    dplyr::group_by(lon, lat) %>%
    tidyr::nest()


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
