#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(magrittr)
library(tidync)

##------------------------------------------------------------------------
## Extract point data and construct separate nested time series data frame
## for each longitde slice
##------------------------------------------------------------------------
dir <- "~/data/alexi_tir/netcdf/"
nclist <- paste0(dir, list.files(dir, pattern = "EDAY_CERES_.?.?.?.?.?.?.?.nc"))
outdir <- "~/data/alexi_tir/data_tidy/"
varnam <- "et"
lonnam <- "lon"
latnam <- "lat"
fileprefix <- "EDAY_CERES_"

# print(as.numeric(args[1]))
nchunk <- 1000  # make sure this is consistent with the number of parallel jobs in the submission script
nlon <- 7200
nrows_chunk <- ceiling(nlon/nchunk)
ilat <- seq(1:nlon)
irow_chunk <- split(ilat, ceiling(seq_along(ilat)/nrows_chunk))

## create files for each longitude slice, containing full time series wrapped for each gridcell (latitude)
rbeni::nclist_to_df(
	nclist = nclist[1], 
	outdir = outdir, 
	fileprefix = fileprefix, 
	varnam = varnam, 
	ilon = irow_chunk[[1]],  # irow_chunk[[as.integer(args[1])]], #as.numeric(args[1]), 
	lonnam = lonnam, 
	latnam = latnam, 
	timenam = "time", 
	ncores = 1, 
	single_basedate = TRUE
	) # 2400  ncores = nnodes * 16; nnodes is requested in submission file 
