#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(multidplyr)
library(rlang)

dir <- "~/data/mct_data/df_whc_hires_chunks/"
filelist <- paste0(dir, list.files(dir, pattern = "df_whc_hires_chunk_.*.RData"))

## function to extract info by file
extract_whc_byfil <- function(ifil){
  load(ifil) # should load 'df_whc'
  df <- df_whc %>% 
    dplyr::ungroup() %>% 
    tidyr::unnest(data_soiltext_top) %>% 
    dplyr::select(lon, lat, fc_top = fc, pwp_top = pwp, whc_top = whc, data_soiltext_sub) %>% 
    tidyr::unnest(data_soiltext_sub) %>% 
    dplyr::select(lon, lat, fc_top, pwp_top, whc_top, fc_sub = fc, pwp_sub = pwp, whc_sub = whc)
  return(df)
}

## get all available cores
ncores <- parallel::detectCores()

if (ncores > 1){
  
  cl <- multidplyr::new_cluster(ncores) %>%
    multidplyr::cluster_library(c("dplyr", "tidyr", "magrittr")) %>%
    multidplyr::cluster_assign(extract_whc_byfil = extract_whc_byfil)
  
  ## distribute to cores, making sure all data from a specific site is sent to the same core
  df_whc <- tibble(ifil = filelist) %>%
    multidplyr::partition(cl) %>%
    dplyr::mutate(out = purrr::map( ifil,
                                    ~extract_whc_byfil(.))) %>% 
    collect() %>% 
    bind_rows() %>% 
    dplyr::select(out) %>% 
    tidyr::unnest(out)
  
} else {
  
  ## testing
  df_whc <- purrr::map(as.list(filelist), 
                       ~extract_whc_byfil(.)) %>% 
    bind_rows() %>% 
    dplyr::select(out) %>% 
    tidyr::unnest(out)
  
}

save(df_whc, file = "~/data/mct_data/df_whc_hires.RData")