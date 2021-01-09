#!/usr/bin/env Rscript

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(multidplyr)
library(rlang)
library(lubridate)

source("R/collect_cwdx_byilon.R")

## get all available cores
ncores <- parallel::detectCores()
#ncores <- 1

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
  df <- purrr::map(as.list(seq(nlon)), ~try(collect_cwdx_byilon(.))) %>% 
    bind_rows()
  
}

## write to file
dirn <- "~/mct/data/"
filn <- paste0("df_cwdx_10_20_40.RData")
path <- paste0(dirn, filn)
print(paste("Writing file:", path))
save(df, file = path)

