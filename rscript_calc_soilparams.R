library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(multidplyr)
library(rbeni)
source("R/calc_soilparams_byilon.R")

nlon <- 7200
ncores <- 2

load("data/df_hwsd_hires.RData") # loads 'df_hwsd'

if (ncores > 1){
  
  cl <- multidplyr::new_cluster(ncores) %>%
    multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr", "rbeni")) %>%
    multidplyr::cluster_assign(calc_soilparams_byilon = calc_soilparams_byilon) %>%
    
    ## distribute to cores, making sure all data from a specific site is sent to the same core
    df_out <- df_hwsd %>%
      slice(100000:100020) %>%       ## xxx test
      group_by(lon, lat) %>% 
      nest() %>% 
      multidplyr::partition(cl) %>%
      dplyr::mutate(out = purrr::map( data,
                                      ~calc_soilparams_byilon(.))) %>% 
      dplyr::collect() %>% 
      dplyr::select(lon, lat, out) %>% 
      tidyr::unnest(out)
    
} else {
  
  df_out <- df_hwsd %>%
    slice(100000:100020) %>%       ## xxx test
    group_by(lon, lat) %>% 
    nest() %>% 
    dplyr::mutate(out = purrr::map( data,
                                    ~calc_soilparams_byilon(.))) %>% 
    dplyr::select(lon, lat, out) %>% 
    tidyr::unnest(out)
  
}
