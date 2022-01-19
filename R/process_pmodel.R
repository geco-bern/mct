#' Process pmodel data
#' 
#' Summarizes pmodel data for MODIS locations
#'
#' @param file pmodel driver file or data frame
#' @param agg aggregate the output 

process_pmodel <- function(
  file,
  agg = FALSE
  ){
  
  # if input file is a dataframe assume
  # rsofun driver file / no classes are assigned
  if(is.character(file)){
    # read in file
    df <- try(readRDS(file))
  } else {
    df <- file
  }
  
  # check succesfull read
  if(inherits(df, "try-error")){
    return(NULL)
  }
  
  # set model parameters
  # WHICH ONES TO USE (Beni)
  params_modl <- list(
    kphio           = 0.09423773,
    soilm_par_a     = 0.33349283,
    soilm_par_b     = 1.45602286,
    tau_acclim_tempstress = 10,
    par_shape_tempstress  = 0.0
  )
  
  # run P-model for the read file / dataframe
  mod <- try(
    suppressWarnings(
      suppressMessages(
        runread_pmodel_f(
          df,
          par = params_modl,
          makecheck = TRUE,
          parallel = FALSE
        )
      )
    )
  )
  
  # check on success of the run
  if(inherits(df, "try-error")){
    return(NULL)
  }
  
  # unnest (flatten) the output dataframe
  mod <- mod %>%
    unnest(cols = c(data))
  
  # grab the site info
  site_info <- df %>%
    dplyr::select(sitename, site_info) %>%
    unnest(cols = c(site_info))
  
  # merge meta-data with site info
  mod <- left_join(mod, site_info)
  
  # grab some of the forcing data
  tmp <- df %>%
    dplyr::select(sitename, forcing) %>%
    unnest(cols = c(forcing)) %>%
    dplyr::select(sitename, date, ppfd)
  
  # join input data and output model data
  mod <- left_join(mod, tmp, by = c("sitename", "date"))
  
  # convert data
  df_out <- mod %>%
    mutate(
      ppfd = ppfd * 60 * 60 * 24,
      apar = fapar * ppfd,
      year = lubridate::year(date),
      doy = lubridate::yday(date),
      alpha = transp/pet,
      rd = 0.015 * vcmax * 60 * 60 * 24 * 12.0107,
      daylength = geosphere::daylength(lat, doy)
    )
  
  # aggregate if so desired
  if (agg){
    
    # don't accumulate after daylength falls below 11 h
    df_out <- df_out %>%
      mutate(
        gpp = ifelse(daylength < 11, 0, gpp),
        apar = ifelse(daylength < 11, 0, apar),
        alpha = ifelse(daylength < 11, NA, alpha),
        rd = ifelse(daylength < 11, 0, rd)
      )
    
    df_out <- df_out %>% 
      group_by(sitename, lat, lon, year) %>% 
      summarise(
        gpp = sum(gpp),
        rd = sum(rd),
        apar = sum(apar),
        alpha = mean(alpha, na.rm = TRUE)
      )
  }
  
  # return output
  return(df_out)
}
