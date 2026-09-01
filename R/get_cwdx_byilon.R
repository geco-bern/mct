get_cwdx_byilon <- function(ilon_hires, df_lat = NULL,
                            config = read_input_config()){
  
  source("R/mct2.R")
  source("R/get_plantwhc_mct_bysite.R")
  
  ## construct output file name
  path <- climate_output_path(
    paste0("data/df_cwdx/df_cwdx_ilon_", ilon_hires, ".RData"),
    config
  )
  ensure_directory(dirname(path))
  
  if (!is.null(df_lat)){
    ##---------------------------------------------------------------------
    ## for redo failed only
    ##---------------------------------------------------------------------
    print(paste("Complementing file:", path))
    if (!file.exists(path)) rlang::abort(paste("Aborting. File does not exist:", path))

    ## Open file with daily water balance
    balance_path <- climate_output_path(
      paste0("data/df_bal/df_bal_ilon_", ilon_hires, ".RData"),
      config
    )
    if (!file.exists(balance_path)) rlang::abort(paste("Aborting. File does not exist:", balance_path))
    load(balance_path) # loads 'df'
    
    ## do it only for latitudes (single cells) where CWDX data was actually found missing
    df_cwdx_corr <- df %>% 
      
      ## round to avoid numerical imprecision
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) %>%

      ## filter to only the ones that are missing
      dplyr::filter(reduce( purrr::map(pull(df_lat, lat), near, x = lat), `|`)) %>%   # "fuzzy filter"
      # dplyr::filter(lat %in% pull(df_lat, lat)) %>% 
      
      ## this runs the function with newly implemented incremental threshhold_drop search 
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

    ## complement cwdx dataframe with corrected one
    load(path)  # loads 'df'
    
    ## correct numerically imprecise digits
    df <- df %>% 
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) 
    
    ## earlier attempt - ignore it
    if ("out_mct_save" %in% names(df)){
      df <- df %>% 
        dplyr::select(-out_mct) %>% 
        rename(out_mct = out_mct_save)
    }

    ## overwrite column with corrected output
    for (idx in seq(nrow(df_cwdx_corr))){
      # idx_full <- which(df$lat == df_cwdx_corr$lat[idx])  # needs a near()
      idx_full <- which(near(df$lat, df_cwdx_corr$lat[idx]))
      df$out_mct[idx_full] <- df_cwdx_corr$out_mct[idx]
    }
    
    ## overwrite file
    print(paste("Overwriting file:", path))
    save(df, file = path) 

    
  } else {
    
    if (!file.exists(path)){
      
      ## Open file with daily water balance
      balance_path <- climate_output_path(
        paste0("data/df_bal/df_bal_ilon_", ilon_hires, ".RData"),
        config
      )
      load(balance_path) # loads 'df'
      
      ## determine CWD and events
      df <- df %>% 
        
        ## round to avoid numerical imprecision
        mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) %>%
        
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
      
    } else {
      print(paste("File exists already:", path))
    } 
    
  }

  error = 0
  return(error)
}
