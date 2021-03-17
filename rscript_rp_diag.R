library(tidyverse)

load("data/df_corr.RData")

calc_return_period <- function(ilon, df_s0){
  
  load(paste0("data/df_cwdx/df_cwdx_ilon_", ilon, ".RData"))
  
  df_s0 %>% 
    mutate(lat = round(lat, digits = 3)) %>% 
    left_join(df %>% 
                mutate(lat = round(lat, digits = 3)), 
              by = "lat") %>% 
    mutate(mod = purrr::map(out_mct, "mod")) %>% 
    dplyr::select(-out_mct) %>% 
    mutate(results = purrr::map(mod, "results")) %>% 
    dplyr::select(-mod) %>% 
    mutate(par = purrr::map(results, "par")) %>% 
    dplyr::select(-results) %>% 
    mutate(loc = purrr::map_dbl(par, "location")) %>% 
    mutate(scale = purrr::map_dbl(par, "scale")) %>% 
    mutate(loc = ifelse(is.null(loc), NA, loc),
           scale = ifelse(is.null(scale), NA, scale)) %>% 
    dplyr::select(-par) %>% 
    rowwise() %>% 
    mutate(rp_diag = calc_return_period_byrow(cwd_lue0_nSIF, loc, scale)) %>% 
    ungroup()
  
}

calc_return_period_byrow <- function(x, loc, scale){
  1 / (1 - exp( -exp(-(x - loc)/scale)))
}

df_rp_diag <- df_corr %>% 
  dplyr::select(lon, lat, cwd_lue0_nSIF) %>% 
  drop_na() %>% 
  group_by(lon) %>% 
  nest() %>% 
  mutate(ilon = as.integer((lon + 179.975)/0.05 + 1)) %>% 
  ungroup() %>% 
  slice(140) %>% 
  mutate(data = purrr::map2(ilon, data, ~calc_return_period(.x, .y))) %>% 
  mutate(data = purrr::map(data, ~dplyr::select(lat, rp_diag))) %>% 
  unnest(data)

save("data/df_rp_diag.RData")