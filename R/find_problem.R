find_problem <- function(uselon, uselat){

  source("R/get_plantwhc_mct_bysite.R")
  
  ## identify which longitude slice to data from
  lon_hires <- seq(-179.975, 179.975, by = 0.05)
  ilon_hires <- which.min(abs(uselon - lon_hires))
  
  ## read bal data
  load(paste0("~/mct/data/df_bal/df_bal_ilon_", as.character(ilon_hires), ".RData"))
  
  ## try getting CWDX again
  df_cwdx <- df %>%
    dplyr::filter(lat == uselat) %>% 
    dplyr::mutate(
      out_mct = purrr::map(
        data,
        ~get_plantwhc_mct_bysite(
          .,
          varname_wbal = "bal",
          varname_date = "time",
          thresh_terminate = 0,
          thresh_drop = 0.9,
          fittype = "Gumbel"))
    ) %>% 
    dplyr::select(-data)
  
  # ## read cwdx data
  # load(paste0("~/mct/data/df_cwdx/df_cwdx_ilon_", as.character(ilon_hires), ".RData"))
  # 
  # df_cwdx <- df %>%
  #   dplyr::filter(lat == uselat)
}