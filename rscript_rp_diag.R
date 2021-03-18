library(tidyverse)

extract_loc <- function(mod){
  loc <- mod$results$par[ "location" ]
  if (!is.null(loc)){
    return(loc)
  } else {
    return(NA)
  }
}

extract_scale <- function(mod){
  scale <- mod$results$par[ "scale" ]
  if (!is.null(scale)){
    return(scale)
  } else {
    return(NA)
  }
}

calc_return_period_byrow <- function(x, loc, scale){
  1 / (1 - exp( -exp(-(x - loc)/scale)))
}

calc_return_period <- function(ilon, df_s0){
  
  load(paste0("data/df_cwdx/df_cwdx_ilon_", ilon, ".RData"))
  
  df_s0 %>% 
    mutate(lat = round(lat, digits = 3)) %>% 
    left_join(df %>% 
                mutate(lat = round(lat, digits = 3)), 
              by = "lat") %>% 
    mutate(mod = purrr::map(out_mct, "mod")) %>% 
    mutate(notavl_mod = purrr::map_lgl(mod, ~identical(NA, .))) %>% 
    dplyr::filter(!notavl_mod) %>% 
    dplyr::select(-out_mct) %>%
    mutate(loc = purrr::map_dbl(mod, ~extract_loc(.)),
           scale = purrr::map_dbl(mod, ~extract_scale(.))) %>% 
    rowwise() %>% 
    mutate(rp_diag = calc_return_period_byrow(cwd_lue0_nSIF, loc, scale)) %>% 
    ungroup()
  
}

load("data/df_corr.RData")

df_rp_diag <- df_corr %>% 
  dplyr::select(lon, lat, cwd_lue0_nSIF) %>% 
  drop_na() %>% 
  group_by(lon) %>% 
  nest() %>% 
  mutate(ilon = as.integer((lon + 179.975)/0.05 + 1)) %>% 
  ungroup() %>% 
  mutate(data = purrr::map2(ilon, data, ~calc_return_period(.x, .y))) %>% 
  mutate(data = purrr::map(data, ~dplyr::select(lat, rp_diag))) %>% 
  unnest(data)

save("data/df_rp_diag.RData")