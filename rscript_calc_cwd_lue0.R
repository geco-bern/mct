#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
# args <- c(3961, 7200)

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(multidplyr)
library(broom)
library(rlang)
library(lubridate)
library(rbeni)
library(segmented)

source("R/calc_cwd_lue0_byilon.R")

##------------------------------------------------------------------------
## split it up into chunks (total number of chunks provided by argument 2)
##------------------------------------------------------------------------
# ## first round
# nlon <- 7200
# ilon <- seq(1:nlon)
# ##----

# ## second round: do only missing ones
# load("./data/df_file_availability_cwd_lue0.RData")
# ilon <- df %>% dplyr::filter(!avl_cwd_lue0) %>% pull(ilon)
# nlon <- length(ilon)

# ## third round: this is created in rscript_collect_cwd_lue0.R
# load("data/vec_ilon_missing.RData") # loads vec_ilon_missing
# ilon <- vec_ilon_missing
# nlon <- length(ilon)

## fourth round:
ilon <- 3961:4176
nlon <- length(ilon)
#----

nchunk <- as.integer(args[2]) # 1000  # make sure this is consistent with the number of parallel jobs (job array!) in the submission script
nrows_chunk <- ceiling(nlon/nchunk)
irow_chunk <- split(ilon, ceiling(seq_along(ilon)/nrows_chunk))

print("getting data for longitude indices:")
print(irow_chunk[[as.integer(args[1])]]) 

## get all available cores
ncores <- parallel::detectCores()

## limit the number of cores to number of individual runs
nruns <- length(irow_chunk[[as.integer(args[1])]])
ncores <- min(ncores, nruns)

# ncores <- 1

if (ncores > 1){

  cl <- multidplyr::new_cluster(ncores) %>%
    multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr", "lubridate", "rlang", "rbeni", "segmented")) %>%
    multidplyr::cluster_assign(calc_cwd_lue0_byilon = calc_cwd_lue0_byilon)

  ## distribute to cores, making sure all data from a specific site is sent to the same core
  df_out <- tibble(ilon = irow_chunk[[as.integer(args[1])]]) %>%
    multidplyr::partition(cl) %>%
    dplyr::mutate(out = purrr::map( ilon,
                                    ~try(calc_cwd_lue0_byilon(., dirn = "~/mct/data/df_cwd_lue0_2"))))

} else {

  ## testing
  df_out <- purrr::map(as.list(irow_chunk[[as.integer(args[1])]]), ~calc_cwd_lue0_byilon(., dirn = "~/mct/data/df_cwd_lue0_2", verbose = FALSE))

}