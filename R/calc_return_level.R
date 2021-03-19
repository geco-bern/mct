calc_return_level <- function(ilon, df_s0){
  
  load(paste0("data/df_cwdx/df_cwdx_ilon_", ilon, ".RData"))
  
  df_s0 %>% 
    mutate(lat = round(lat, digits = 3)) %>% 
    left_join(df %>% 
                mutate(lat = round(lat, digits = 3)), 
              by = "lat") %>% 
    dplyr::select(-lon) %>% 
    mutate(mod = purrr::map(out_mct, "mod")) %>% 
    mutate(notavl_mod = purrr::map_lgl(mod, ~identical(NA, .))) %>% 
    dplyr::filter(!notavl_mod) %>% 
    dplyr::select(-out_mct) %>%
    mutate(df_rl = purrr::map2(s0, mod, ~f_calc_return_level(.x, .y))) %>% 
    unnest(df_rl) %>% 
    mutate(logbias = log(s0 / return_level))

}

f_calc_return_level <- function(x, mod){

  return_period <- c(seq(2, 9, by = 1), seq(10, 95, by = 5), seq(100, 300, by = 10), seq(400, 1000, by = 100))
  
  return_level <- extRemes::return.level(
    mod, 
    return.period = return_period)
  
  df_return_level <- tibble( 
    return_period = return_period, 
    return_level = unname(c(return_level)))
  
  # return_period_best <- df_return_level %>% 
  #   dplyr::filter(return_level == return_level[which.min(abs(return_level - x))]) %>% 
  #   pull(return_period)
  
  return(df_return_level)
}

