#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(magrittr)
library(tidync)
library(rbeni)

##------------------------------------------------------------------------
## Extract point data and construct separate nested time series data frame
## for each longitde slice
##------------------------------------------------------------------------
dir <- "~/data/alexi_tir/netcdf/"
nclist <- paste0(dir, list.files(dir, pattern = "EDAY_CERES_.?.?.?.?.nc"))
outdir <- "~/data/alexi_tir/data_tidy/"
varnam <- "et"
lonnam <- "lon"
latnam <- "lat"
fileprefix <- "EDAY_CERES_"

##------------------------------------------------------------------------
## split it up into chunks (total number of chunks provided by argument 2)
##------------------------------------------------------------------------
# nchunk <- as.integer(args[2]) # 1000  # make sure this is consistent with the number of parallel jobs (job array!) in the submission script
# nlon <- 7200
# nrows_chunk <- ceiling(nlon/nchunk)
# vec_ilon <- seq(1:nlon)
# irow_chunk <- split(vec_ilon, ceiling(seq_along(vec_ilon)/nrows_chunk))
# print("getting data for longitude indices:")
# print(irow_chunk[[as.integer(args[1])]]) 

## Second round
nchunk <- as.integer(args[2]) # 1000  # make sure this is consistent with the number of parallel jobs (job array!) in the submission script
vec_ilon <- 3594:3598
nrows_chunk <- ceiling(length(vec_ilon)/nchunk)
irow_chunk <- split(vec_ilon, ceiling(seq_along(vec_ilon)/nrows_chunk))
print("getting data for longitude indices:")
print(irow_chunk[[as.integer(args[1])]]) 

## create files for each longitude slice, containing full time series wrapped for each gridcell (latitude)
nclist_to_df(
	nclist = nclist, 
	outdir = outdir, 
	fileprefix = fileprefix, 
	varnam = varnam, 
	ilon = irow_chunk[[as.integer(args[1])]],
	lonnam = lonnam, 
	latnam = latnam, 
	timenam = "time", 
	ncores = 4, 
	single_basedate = TRUE
	)
