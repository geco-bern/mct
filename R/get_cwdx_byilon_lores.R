get_cwdx_byilon_lores <- function(ilon, config = read_input_config()){
  
  source("R/mct2.R")
  source("R/get_plantwhc_mct_bysite.R")
  
  ## construct output file name
  path <- climate_output_path(
    paste0("data/df_cwdx_lores/df_cwdx_ilon_", ilon, ".RData"),
    config
  )
  ensure_directory(dirname(path))
  
  if (!file.exists(path)){
    
    ## Open file with daily water balance
    balance_path <- climate_output_path(
      paste0("data/df_bal_lores/df_bal_ilon_", ilon, ".RData"),
      config
    )
    load(balance_path) # loads 'df'
    
    ## determine CWD and events
    df <- df %>% 
      
      ## use only the configured common analysis period
      mutate(data = purrr::map(
        data,
        ~dplyr::filter(
          .,
          lubridate::year(time) >= config$analysis_period$start_year,
          lubridate::year(time) <= config$analysis_period$end_year
        )
      )) %>%
      
      ## round to avoid numerical imprecision
      mutate(lon = round(lon, digits = 2), lat = round(lat, digits = 2)) %>%
      
      dplyr::mutate(
        out_mct = purrr::map(
          data,
          ~get_plantwhc_mct_bysite(
            .,
            varname_wbal = "bal",
            varname_date = "time",
            thresh_terminate = 0.0,
            thresh_drop = 0.9,
            fittype = "Gumbel"))
      ) %>% 
      dplyr::select(-data)
    
    print(paste("Writing file:", path))
    save(df, file = path)
    
  }

  error = 0
  return(error)
}
