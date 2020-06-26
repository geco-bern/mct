#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(multidplyr)
library(rbeni)

source("R/calc_soilparams_byilon.R")

load("data/df_hwsd_hires.RData") # loads 'df_hwsd'

##------------------------------------------------------------------------
## split it up into chunks (total number of chunks provided by argument 2)
##------------------------------------------------------------------------
nchunk <- as.integer(args[2]) # 1000  # make sure this is consistent with the number of parallel jobs (job array!) in the submission script
nlon <- 7200
nrows_chunk <- ceiling(nlon/nchunk)
ilat <- seq(1:nlon)
irow_chunk <- split(ilat, ceiling(seq_along(ilat)/nrows_chunk))

print("getting data for longitude indices:")
print(irow_chunk[[as.integer(args[1])]]) 

## get all available cores
ncores <- parallel::detectCores()

if (ncores > 1){
  
  cl <- multidplyr::new_cluster(ncores) %>%
    multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr", "rbeni")) %>%
    multidplyr::cluster_assign(calc_soilparams_byilon = calc_soilparams_byilon) %>%
    
  ## distribute to cores, making sure all data from a specific site is sent to the same core
  df_out <- df_hwsd %>%
    ungroup() %>%
    mutate(idx = 1:n()) %>% 
    dplyr::filter(idx %in% irow_chunk[[as.integer(args[1])]]) %>%
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
    # slice(100000:100020) %>%       ## xxx test
    ungroup() %>%
    mutate(idx = 1:n()) %>% 
    dplyr::filter(idx %in% irow_chunk[[as.integer(args[1])]]) %>%
    group_by(lon, lat) %>% 
    nest() %>% 
    dplyr::mutate(out = purrr::map( data,
                                    ~calc_soilparams_byilon(.))) %>% 
    dplyr::select(lon, lat, out) %>% 
    tidyr::unnest(out)
  
}
