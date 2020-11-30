#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
#args <- c(4901, 7200)

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(multidplyr)

source("R/get_et_mm_bylon.R")

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
    multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr", "rbeni", "ncdf4", "raster", "sp")) %>%
    multidplyr::cluster_assign(get_et_mm_byilon = get_et_mm_byilon)

	## distribute to cores, making sure all data from a specific site is sent to the same core
	df_out <- tibble(ilon = irow_chunk[[as.integer(args[1])]]) %>%
	  multidplyr::partition(cl) %>%
	  dplyr::mutate(out = purrr::map( ilon,
	                                  ~get_et_mm_byilon(.)))

} else {

  df_out <- purrr::map(
  	as.list(irow_chunk[[as.integer(args[1])]]),
  	~get_et_mm_byilon(.)
  	)

}

# ##------------------------------------------------------------------------
# ## second round
# ##------------------------------------------------------------------------
# source("rscript_check_files.R")
# load("data/df_file_availability.RData")
# ilon <- df %>% 
#   dplyr::filter(!avl_et_mm) %>% 
#   pull(ilon)
# 
# # nchunk <- as.integer(args[2]) # 1000  # make sure this is consistent with the number of parallel jobs (job array!) in the submission script
# # nlon <- length(ilon)
# # nrows_chunk <- ceiling(nlon/nchunk)
# # irow_chunk <- split(ilon, ceiling(seq_along(ilon)/nrows_chunk))
# 
# ## get all available cores
# ncores <- parallel::detectCores()
# 
# if (ncores > 1){
# 
#   cl <- multidplyr::new_cluster(ncores) %>%
#     multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr", "rbeni", "ncdf4", "raster", "sp")) %>%
#     multidplyr::cluster_assign(get_et_mm_byilon = get_et_mm_byilon)
# 
# 	## distribute to cores, making sure all data from a specific site is sent to the same core
# 	df_out <- tibble(ilon = ilon) %>%
# 	  multidplyr::partition(cl) %>%
# 	  dplyr::mutate(out = purrr::map( ilon,
# 	                                  ~try(get_et_mm_byilon(.))))
# 
# } else {
# 
#   df_out <- purrr::map(
#   	as.list(ilon),
#   	~try(get_et_mm_byilon(.))
#   	)
# 
# }
