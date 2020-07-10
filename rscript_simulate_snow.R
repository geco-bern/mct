#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(multidplyr)
source("R/simulate_snow_byilon.R")

##------------------------------------------------------------------------
## split it up into chunks (total number of chunks provided by argument 2)
##------------------------------------------------------------------------
nchunk <- as.integer(args[2]) # 1000  # make sure this is consistent with the number of parallel jobs (job array!) in the submission script
nlon <- 720
nrows_chunk <- ceiling(nlon/nchunk)
ilat <- seq(1:nlon)
irow_chunk <- split(ilat, ceiling(seq_along(ilat)/nrows_chunk))

print("getting data for longitude indices:")
print(irow_chunk[[as.integer(args[1])]]) 

## get all available cores
ncores <- parallel::detectCores()

# if (ncores > 1){
#   
#   cl <- multidplyr::new_cluster(ncores) %>%
#     multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr")) %>%
#     multidplyr::cluster_assign(simulate_snow_byilon = simulate_snow_byilon)
#     
#   ## distribute to cores, making sure all data from a specific site is sent to the same core
#   df_out <- tibble(ilon = irow_chunk[[as.integer(args[1])]]) %>%
#     multidplyr::partition(cl) %>%
#     dplyr::mutate(out = purrr::map( ilon,
#                                     ~simulate_snow_byilon(.)))
#     
# } else {
#   
#   df_out <- purrr::map(
#   	as.list(irow_chunk[[as.integer(args[1])]]), 
#   	~simulate_snow_byilon(.)
#   	)
#   
# }


##------------------------------------------------------------------------
## second round
##------------------------------------------------------------------------
source("rscript_check_files.R")
load("data/df_file_availability.RData")
ilon <- df %>% 
  dplyr::filter(!avl_snow) %>% 
  pull(ilon)

## get all available cores
ncores <- parallel::detectCores()

if (ncores > 1){
  
  cl <- multidplyr::new_cluster(ncores) %>%
    multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr")) %>%
    multidplyr::cluster_assign(simulate_snow_byilon = simulate_snow_byilon)
  
  ## distribute to cores, making sure all data from a specific site is sent to the same core
  df_out <- tibble(ilon = ilon) %>%
    multidplyr::partition(cl) %>%
    dplyr::mutate(out = purrr::map( ilon,
                                    ~try(simulate_snow_byilon(.))))
  
} else {
  
  df_out <- purrr::map(
    as.list(ilon), 
    ~try(simulate_snow_byilon(.))
  )
  
}

