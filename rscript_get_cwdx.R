#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
args <- c(3421, 7200)

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
    multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr", "extRemes", "lubridate", "rlang", "broom")) %>%
    multidplyr::cluster_assign(get_cwdx_byilon = get_cwdx_byilon)

  ## distribute to cores, making sure all data from a specific site is sent to the same core
  df_out <- tibble(ilon = irow_chunk[[as.integer(args[1])]]) %>%
    multidplyr::partition(cl) %>%
    dplyr::mutate(out = purrr::map( ilon,
                                    ~try(get_cwdx_byilon(.))))

} else {

  ## testing
  df_out <- purrr::map(as.list(irow_chunk[[as.integer(args[1])]]), ~try(get_cwdx_byilon(.)))

}

# ##------------------------------------------------------------------------
# ## third round - some cwdx were missing because of continuously accumulating
# ## CWD. Added flexibility in terminate threshold.
# ##------------------------------------------------------------------------
# source("R/complement_cwdx.R")
# source("R/get_plantwhc_mct_bysite.R")
# source("R/mct2.R")

# ## identify cells with missing value for cwdx20
# load("data/df_cwdx_10_20_40.RData") # loads 'df'
# df_missing <- df %>% 
#   dplyr::filter(is.na(cwdx20)) %>% 
#   dplyr::select(lon, lat) %>% 
#   group_by(lon) %>% 
#   nest()

# if (ncores > 1){
  
#   cl <- multidplyr::new_cluster(ncores) %>%
#     multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr", "extRemes", "lubridate", "rlang", "broom")) %>%
#     multidplyr::cluster_assign(mct = mct) %>% 
#     multidplyr::cluster_assign(get_plantwhc_mct_bysite = get_plantwhc_mct_bysite) %>% 
#     multidplyr::cluster_assign(complement_cwdx = complement_cwdx)
  
#   ## distribute to cores, making sure all data from a specific site is sent to the same core
#   df_out <- df_missing %>%
#     multidplyr::partition(cl) %>%
#     dplyr::mutate(out = purrr::map2( lon, data,
#                                     ~try(complement_cwdx(.x, .y))))
  
# } else {
  
#   ## testing
#   df_out <- df_missing %>%
#     dplyr::mutate(out = purrr::map2( lon, data,
#                                      ~try(complement_cwdx(.x, .y))))
  
# }

# ##------------------------------------------------------------------------
# ## second round
# ##------------------------------------------------------------------------
# source("rscript_check_files.R")
# load("data/df_file_availability.RData")
# ilon <- df %>% 
#   dplyr::filter(!avl_cwdx) %>% 
#   pull(ilon)
# 
# if (ncores > 1){
# 
#   cl <- multidplyr::new_cluster(ncores) %>%
#     multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr", "extRemes", "lubridate", "rlang", "broom")) %>%
#     multidplyr::cluster_assign(get_cwdx_byilon = get_cwdx_byilon)
# 
#   ## distribute to cores, making sure all data from a specific site is sent to the same core
#   df_out <- tibble(ilon = ilon) %>%
#     multidplyr::partition(cl) %>%
#     dplyr::mutate(out = purrr::map( ilon,
#                                     ~try(get_cwdx_byilon(.))))
# 
# } else {
# 
#   ## testing
#   df_out <- purrr::map(as.list(ilon), ~try(get_cwdx_byilon(.)))
# 
# }
