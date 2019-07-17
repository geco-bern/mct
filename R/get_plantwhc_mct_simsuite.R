get_plantwhc_mct_simsuite <- function(df_sites, dir_climate, dir_fapar){

  ## extract all the climate data (sofun output)
  df <- tibble( year = 1982:2011 ) %>% 
    rowwise() %>%
    dplyr::mutate(filn_pet  = paste0( dir_climate, "/s1_fapar3g_v4_global.d.pet",  year, ".nc"),
                  filn_wbal = paste0( dir_climate, "/s1_fapar3g_v4_global.d.wbal", year, ".nc")) %>%
    dplyr::mutate(data_pet  = purrr::map(filn_pet,  ~extract_pointdata_allsites(., df_sites, get_time = TRUE)),
                  data_wbal = purrr::map(filn_wbal, ~extract_pointdata_allsites(., df_sites, get_time = TRUE)))
  
  ## re-arrange data to a flat table
  ddf <- df %>% 
    dplyr::select(-filn_pet, -filn_wbal) %>% 
    tidyr::unnest(data_pet) %>% 
    dplyr::select(-year_start, -year_end, -year) %>% 
    tidyr::unnest(data) %>% 
    dplyr::rename(pet = V1) %>% 
    left_join(
      df %>% 
        dplyr::select(-filn_pet, -filn_wbal) %>% 
        tidyr::unnest(data_wbal) %>% 
        dplyr::select(-year_start, -year_end, -year) %>% 
        tidyr::unnest(data) %>% 
        dplyr::rename(water_to_soil = V1),
      by = c("lon", "lat", "sitename", "elv", "date")
    ) %>% 
    dplyr::select(-lon, -lat, -elv)

  ## read all the fapar data (interpolated to daily)
  ddf <- df_sites %>% 
    # slice(1) %>%
    dplyr::mutate( filn = paste0( dir_fapar, "/", sitename, "dfapar_MODIS_FPAR_MCD15A3H_gee_MCD15A3H_", sitename, "_gee_subset.csv" ) ) %>% 
    dplyr::mutate( data_fapar = purrr::map(filn, ~read_csv(.)) ) %>% 
    tidyr::unnest( data_fapar ) %>% 
    dplyr::select(sitename, date, fapar = modisvar_interpol ) %>% 
    dplyr::right_join(ddf, by = c("sitename", "date"))
  
  # library(ggplot2)
  # ddf %>% 
  #   dplyr::filter(lubridate::year(date) %in% 2000:2005) %>% 
  #   ggplot(aes(x=date, y=wbal - fapar * pet)) +
  #   geom_line()
  
  ## nest data frames per site and apply MCT function
  ddf <- ddf %>% 
    dplyr::mutate(wbal = water_to_soil - fapar * pet) %>% 
    tidyr::drop_na(wbal) %>% 
    group_by(sitename) %>% 
    tidyr::nest() %>% 
    # slice(1) %>% 
    dplyr::mutate( out_mct = purrr::map(data, ~get_plantwhc_mct_bysite(., varname_wbal = "wbal")) )
    
  return(ddf)
}


##--------------------------------------------------------------------
## Extracts point data for a set of sites given by df_lonlat using
## functions from the raster package.
##--------------------------------------------------------------------
extract_pointdata_allsites <- function( filename, df_lonlat, get_time = FALSE ){
  
  ## load file using the raster library
  print(paste("Creating raster brick from file", filename))
  rasta <- raster::brick(filename)
  
  df_lonlat <- raster::extract(rasta, sp::SpatialPoints(dplyr::select(df_lonlat, lon, lat)), sp = TRUE) %>% 
    as_tibble() %>% 
    tidyr::nest(-lon, -lat) %>%
    right_join(df_lonlat, by = c("lon", "lat")) %>%
    mutate( data = purrr::map(data, ~dplyr::slice(., 1)) ) %>% 
    dplyr::mutate(data = purrr::map(data, ~t(.))) %>% 
    dplyr::mutate(data = purrr::map(data, ~as_tibble(.)))
  
  if (get_time){
    timevals <- raster::getZ(rasta)
    df_lonlat <- df_lonlat %>% 
      mutate( data = purrr::map(data, ~bind_cols(., tibble(date = timevals))))
  }
  
  return(df_lonlat)
}


