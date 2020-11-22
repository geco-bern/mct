#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
# args <- c(1153,7200)

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(multidplyr)
library(broom)
library(rlang)
library(lubridate)
library(extRemes)

source("R/get_cwdx_byilon.R")

## get all available cores
ncores <- parallel::detectCores()

##------------------------------------------------------------------------
## Determine failed cells
##------------------------------------------------------------------------
load("data/df_cwdx_10_20_40.RData") # loads 'df', created by rscript_collect_cwdx.R

lon_hires <- seq(-179.975, 179.975, by = 0.05)

df <- df %>% 
  
  ## filter failed
  dplyr::filter(is.na(cwdx20)) %>% 
  
  ## exclude polar regions
  dplyr::filter(lat > -60 & lat < 75) %>% 
  
  ## some had weird longitude values
  mutate(lon = round(lon, digits = 3)) %>% 
  
  dplyr::select(lon, lat) %>% 
  
  ## determine ilon
  rowwise() %>% 
  #mutate(ilon_hires = which.min(abs(lon_hires - lon))) %>% 
  mutate(ilon_hires = (lon + 179.975)/0.05 + 1)

df_nested <- df %>% 
  dplyr::select(lat, ilon_hires) %>% 
  group_by(ilon_hires) %>% 
  nest() %>% 
  mutate(ncells = purrr::map_dbl(data, ~nrow(.))) %>% 
  rename(ilon = ilon_hires)

## split it up into chunks (total number of chunks provided by argument 2)
nchunk <- as.integer(args[2]) # 1000  # make sure this is consistent with the number of parallel jobs (job array!) in the submission script
ilon_vec <- df %>% pull(ilon_hires) %>% unique()
nrows_chunk <- ceiling(length(ilon_vec)/nchunk)
irow_chunk <- split(ilon_vec, ceiling(seq_along(ilon_vec)/nrows_chunk))

print("getting data for longitude indices:")
print(irow_chunk[[as.integer(args[1])]]) 

##------------------------------------------------------------------------
## first level: based on daily water balance, re-do get_cwdx_byilon(.) with incrementally increasing thresh_terminate
##------------------------------------------------------------------------
if (ncores > 1){
  
  cl <- multidplyr::new_cluster(ncores) %>%
    multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr", "extRemes", "lubridate", "rlang", "broom")) %>%
    multidplyr::cluster_assign(get_cwdx_byilon = get_cwdx_byilon)

  ## distribute to cores, making sure all data from a specific site is sent to the same core
  df_out <- tibble(ilon = irow_chunk[[as.integer(args[1])]]) %>%
    left_join(df_nested, by = "ilon") %>% 
    multidplyr::partition(cl) %>%
    dplyr::mutate(out = purrr::map2( ilon, data,
                                    ~try(get_cwdx_byilon(.x, df_lat = .y))))
  
} else {
  
  ## testing
  df_out <- tibble(ilon = irow_chunk[[as.integer(args[1])]]) %>%
  #df_out <- tibble(ilon = 721) %>%
    left_join(df_nested, by = "ilon") %>% 
    dplyr::mutate(out = purrr::map2( ilon, data,
                                     ~get_cwdx_byilon(.x, df_lat = .y)))
  
}
