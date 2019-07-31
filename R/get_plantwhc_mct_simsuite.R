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
  myread_csv <- function(filn){
    if (file.exists(filn)){
      df <- read_csv(filn) %>% 
        dplyr::select(date, fapar = modisvar_interpol )
    } else {
      df <- tibble(date = NA, fapar = NA)
    }
    return(df)
  }
  
  ddf <- df_sites %>% 
    # slice(1:10) %>%
    dplyr::mutate( filn = paste0( dir_fapar, "/", sitename, "dfapar_MODIS_FPAR_MCD15A3H_gee_MCD15A3H_", sitename, "_gee_subset.csv" ) ) %>% 
    dplyr::mutate( data_fapar = purrr::map(filn, ~myread_csv(.)) ) %>% 
    tidyr::unnest( data_fapar ) %>% 
    tidyr::drop_na(fapar) %>% 
    dplyr::select(sitename, date, fapar) %>% 
    dplyr::right_join(ddf, by = c("sitename", "date"))
  
  ## Set fapar to zero where pet is zero (arctic night?)
  ddf <- ddf %>%
    rowwise() %>% 
    dplyr::mutate(fapar = ifelse(pet==0 & is.na(fapar), 0, fapar))
  
  # library(ggplot2)
  # ddf %>% 
  #   dplyr::filter(lubridate::year(date) %in% 2000:2005) %>% 
  #   ggplot(aes(x=date, y=wbal - fapar * pet)) +
  #   geom_line()
  
  ## calculate wbal and nest data frames per site
  ddf <- ddf %>% 
    dplyr::mutate(wbal = water_to_soil - fapar * pet) %>% 
    tidyr::drop_na(wbal) %>% 
    group_by(sitename) %>% 
    tidyr::nest() 
  
  ## remove sites where not the full time series is available
  ddf <- ddf %>%
    dplyr::mutate(len = purrr::map_int(data, ~nrow(.))) %>% 
    dplyr::filter(len == 10948)

  ## apply MCT function
  ddf <- ddf %>% 
    dplyr::mutate( out_mct = purrr::map(data, ~get_plantwhc_mct_bysite(., varname_wbal = "wbal")) )
    
  return(ddf)
}


