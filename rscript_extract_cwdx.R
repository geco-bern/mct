#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(multidplyr)
library(rlang)
library(lubridate)

source("R/extract_cwdx_byilon.R")
source("R/collect_cwdx_byilon.R")

## get all available cores
ncores <- parallel::detectCores()

##------------------------------------------------------------------------
## 1. extract return levels only and write to (small) files
##------------------------------------------------------------------------
nchunk <- as.integer(args[2]) # 1000  # make sure this is consistent with the number of parallel jobs (job array!) in the submission script
nlon <- 7200
nrows_chunk <- ceiling(nlon/nchunk)
ilat <- seq(1:nlon)
irow_chunk <- split(ilat, ceiling(seq_along(ilat)/nrows_chunk))

print("getting data for longitude indices:")
print(irow_chunk[[as.integer(args[1])]]) 

if (ncores > 1){
  
  cl <- multidplyr::new_cluster(ncores) %>%
    multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr", "rlang")) %>%
    multidplyr::cluster_assign(extract_cwdx_byilon = extract_cwdx_byilon)
  
  ## distribute to cores, making sure all data from a specific site is sent to the same core
  df <- tibble(ilon = irow_chunk[[as.integer(args[1])]]) %>%
    multidplyr::partition(cl) %>%
    dplyr::mutate(out = purrr::map( ilon,
                                    ~extract_cwdx_byilon(.)))
  
} else {
  
  ## testing
  df <- purrr::map(as.list(irow_chunk[[as.integer(args[1])]]), ~extract_cwdx_byilon(.))
  
}

##------------------------------------------------------------------------
## 2. collect data from small files into a single dataframe
##------------------------------------------------------------------------
nlon <- 7200

if (ncores > 1){
  
  cl <- multidplyr::new_cluster(ncores) %>%
    multidplyr::cluster_assign(collect_cwdx_byilon = collect_cwdx_byilon)
  
  ## distribute to cores, making sure all data from a specific site is sent to the same core
  df <- tibble(ilon = seq(nlon)) %>%
    multidplyr::partition(cl) %>%
    dplyr::mutate(data = purrr::map( ilon,
                                    ~collect_cwdx_byilon(.))) %>% 
    collect() %>% 
    tidyr::unnest(data)
  
} else {
  
  ## testing
  df <- purrr::map(as.list(seq(nlon)), ~collect_cwdx_byilon(.))
  
}

## write to file
dirn <- "~/mct/data/"
filn <- paste0("df_cwdx_10_20_40.RData")
path <- paste0(dirn, filn)
print(paste("Writing file:", path))
save(df, file = path)

