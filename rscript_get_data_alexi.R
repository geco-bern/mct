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
nclist <- paste0(dir, list.files(dir, pattern = "EDAY_CERES_.?.?.?.?.nc"))
outdir <- "~/data/alexi_tir/data_tidy/"
varnam <- "et"
lonnam <- "lon"
latnam <- "lat"
fileprefix <- "EDAY_CERES_"

# print(as.numeric(args[1]))

## create files for each longitude slice, containing full time series wrapped for each gridcell (latitude)
<<<<<<< HEAD
rbeni::nclist_to_df(nclist, outdir, fileprefix, varnam, lonnam, latnam, timenam = "time", ncores = 512, single_basedate = TRUE) # 2400  ncores = nnodes * 16; nnodes is requested in submission file 
=======
rbeni::nclist_to_df(
	nclist = nclist, 
	outdir = outdir, 
	fileprefix = fileprefix, 
	varnam = varnam, 
	ilon = 1, #as.numeric(args[1]), 
	lonnam = lonnam, 
	latnam = latnam, 
	timenam = "time", 
	ncores = 1, 
	single_basedate = TRUE
	) # 2400  ncores = nnodes * 16; nnodes is requested in submission file 
>>>>>>> be7bcbb6d208e00bff3e2747817de1b382cb7c1c
