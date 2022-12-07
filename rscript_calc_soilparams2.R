#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
#args <- c(5000, 7200)

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(multidplyr)
library(rbeni)

source("R/calc_soilparams_byilon.R")

# path <- paste0("~/mct/data/df_whc_hires_chunks/df_whc_hires_ilon_", as.integer(args[1]), ".RData")

# if (!file.exists(path)){

  # load("data/df_hwsd_hires.RData") # loads 'df_hwsd', prepared directly in workflow.Rmd
  # df_hwsd <- df_hwsd %>%
  #   mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3))
  # ilon <- (df_hwsd$lon + 179.975)/0.05 + 1
  # df_hwsd <- df_hwsd %>% 
  #   mutate(ilon = ilon)
  
    # left_join(
    #   tibble(
    #     ilon = 1:7200,
    #     lon = seq(-179.975, 179.975, by = 0.05)
    #   ),
    #   by = "lon"
    # )
    
##------------------------------------------------------------------------
## split it up into chunks (total number of chunks provided by argument 2)
##------------------------------------------------------------------------
nchunk <- as.integer(args[2]) # 1000  # make sure this is consistent with the number of parallel jobs (job array!) in the submission script
# nlon <- nrow(df_hwsd)
nlon <- 7200
nrows_chunk <- ceiling(nlon/nchunk)
ilon <- seq(1:nlon)
irow_chunk <- split(ilon, ceiling(seq_along(ilon)/nrows_chunk))

print("getting data for longitude indices:")
print(irow_chunk[[as.integer(args[1])]]) 

## get all available cores
ncores <- parallel::detectCores()

## limit the number of cores to number of individual runs
nruns <- length(irow_chunk[[as.integer(args[1])]])
ncores <- min(ncores, nruns)  

if (ncores > 1){
  
  cl <- multidplyr::new_cluster(ncores) %>%
    multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr", "rbeni")) %>%
    multidplyr::cluster_assign(calc_soilparams_byilon = calc_soilparams_byilon)
  
  ## distribute to cores, making sure all data from a specific site is sent to the same core
  df_out <- tibble(ilon = irow_chunk[[as.integer(args[1])]]) %>%
    multidplyr::partition(cl) %>%
    dplyr::mutate(out = purrr::map( ilon,
                                    ~calc_soilparams_byilon(.)))
  
} else {
  
  ## testing
  df_out <- purrr::map(
    as.list(irow_chunk[[as.integer(args[1])]]), 
    ~calc_soilparams_byilon(.))
  
  # df_whc <- df_hwsd %>%
  #   mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) %>% 
  #   ungroup() %>%
  #   dplyr::filter(ilon %in% irow_chunk[[as.integer(args[1])]]) %>%
  #   tidyr::pivot_longer(cols = 3:12, names_to = "var")
  #   group_by(lon, lat) %>% 
  #   nest() %>% 
  #   dplyr::mutate(out = purrr::map( data,
  #                                   ~calc_soilparams_byilon(.))) %>% 
  #   dplyr::select(lon, lat, out) %>% 
  #   tidyr::unnest(out)
  
}

