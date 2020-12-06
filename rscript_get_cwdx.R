#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
# args <- c(3421, 7200)

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

# ##------------------------------------------------------------------------
# ## split it up into chunks (total number of chunks provided by argument 2)
# ##------------------------------------------------------------------------
# nchunk <- as.integer(args[2]) # 1000  # make sure this is consistent with the number of parallel jobs (job array!) in the submission script
# nlon <- 7200
# nrows_chunk <- ceiling(nlon/nchunk)
# vec_ilon <- seq(1:nlon)
# irow_chunk <- split(vec_ilon, ceiling(seq_along(vec_ilon)/nrows_chunk))
# 
# print("getting data for longitude indices:")
# print(irow_chunk[[as.integer(args[1])]]) 
# 
# ## get all available cores
# ncores <- parallel::detectCores()
#
# if (ncores > 1){
# 
#   cl <- multidplyr::new_cluster(ncores) %>%
#     multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr", "extRemes", "lubridate", "rlang", "broom")) %>%
#     multidplyr::cluster_assign(get_cwdx_byilon = get_cwdx_byilon)
# 
#   ## distribute to cores, making sure all data from a specific site is sent to the same core
#   df_out <- tibble(ilon = irow_chunk[[as.integer(args[1])]]) %>%
#     multidplyr::partition(cl) %>%
#     dplyr::mutate(out = purrr::map( ilon,
#                                     ~try(get_cwdx_byilon(.))))
# 
# } else {
# 
#   ## testing
#   df_out <- purrr::map(as.list(irow_chunk[[as.integer(args[1])]]), ~try(get_cwdx_byilon(.)))
# 
# }

##------------------------------------------------------------------------
## second round
##------------------------------------------------------------------------
load("data/df_file_availability.RData")
vec_ilon <- df %>%
  dplyr::filter(!avl_cwdx) %>%
  pull(ilon)

nchunk <- as.integer(args[2])
nrows_chunk <- ceiling(length(vec_ilon)/nchunk)
irow_chunk <- split(vec_ilon, ceiling(seq_along(vec_ilon)/nrows_chunk))

print("getting data for longitude indices:")
print(irow_chunk[[as.integer(args[1])]]) 

## get all available cores
ncores <- parallel::detectCores()

if (ncores > 1){

  cl <- multidplyr::new_cluster(ncores) %>%
    multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr", "extRemes", "lubridate", "rlang", "broom")) %>%
    multidplyr::cluster_assign(get_cwdx_byilon = get_cwdx_byilon)

  ## distribute to cores, making sure all data from a specific site is sent to the same core
  df_out <- tibble(ilon = ilon) %>%
    multidplyr::partition(cl) %>%
    dplyr::mutate(out = purrr::map( irow_chunk[[as.integer(args[1])]],
                                    ~try(get_cwdx_byilon(.))))

} else {

  ## testing
  df_out <- purrr::map(as.list(irow_chunk[[as.integer(args[1])]]), ~try(get_cwdx_byilon(.)))

}
