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

nlon <- 7200

## get all available cores
ncores <- parallel::detectCores()

if (ncores > 1){
  
  cl <- multidplyr::new_cluster(ncores) %>%
    multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr", "rlang")) %>%
    multidplyr::cluster_assign(extract_cwdx_byilon = extract_cwdx_byilon)
    
  ## distribute to cores, making sure all data from a specific site is sent to the same core
  df <- tibble(ilon = seq(nlon)) %>%
    multidplyr::partition(cl) %>%
    dplyr::mutate(out = purrr::map( ilon,
                                    ~extract_cwdx_byilon(.))) %>% 
    collect() %>% 
    tidyr::unnest(out)
    
} else {
  
  ## testing
  df <- purrr::map(as.list(irow_chunk[[as.integer(args[1])]]), ~extract_cwdx_byilon(.))
  
}

## write to file
dirn <- "~/mct/data/"
filn <- paste0("df_cwdx_10_20_40_ilon_", ilon_hires, ".RData")
if (!dir.exists(dirn)) system("mkdir -p ~/mct/data")
path <- paste0(dirn, filn)
print(paste("Writing file:", path))
save(df, file = path)

