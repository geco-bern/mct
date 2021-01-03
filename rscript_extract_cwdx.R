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
                                    ~try(extract_cwdx_byilon(., overwrite = FALSE))))

} else {

  ## testing
  df <- purrr::map(as.list(irow_chunk[[as.integer(args[1])]]), ~try(extract_cwdx_byilon(., overwrite = FALSE)))

}

# ##------------------------------------------------------------------------
# ## second round
# ##------------------------------------------------------------------------
# load("data/df_file_availability.RData")
# ilon <- df %>%
#   dplyr::filter(!avl_cwdx_10_20_40) %>%
#   pull(ilon)
# 
# nchunk <- as.integer(args[2]) # 1000  # make sure this is consistent with the number of parallel jobs (job array!) in the submission script
# nrows_chunk <- ceiling(length(ilon)/nchunk)
# irow_chunk <- split(ilon, ceiling(seq_along(ilon)/nrows_chunk))
# 
# print("getting data for longitude indices:")
# print(irow_chunk[[as.integer(args[1])]]) 
# 
# if (ncores > 1){
#   
#   cl <- multidplyr::new_cluster(ncores) %>%
#     multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr", "rlang")) %>%
#     multidplyr::cluster_assign(extract_cwdx_byilon = extract_cwdx_byilon)
#   
#   ## distribute to cores, making sure all data from a specific site is sent to the same core
#   df <- tibble(ilon = irow_chunk[[as.integer(args[1])]]) %>%
#     multidplyr::partition(cl) %>%
#     dplyr::mutate(out = purrr::map( ilon,
#                                     ~try(extract_cwdx_byilon(., overwrite = FALSE))))
#   
# } else {
#   
#   ## testing
#   df <- purrr::map(as.list(irow_chunk[[as.integer(args[1])]]), ~try(extract_cwdx_byilon(., overwrite = FALSE)))
#   
# }