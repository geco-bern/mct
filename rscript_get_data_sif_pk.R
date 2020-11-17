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
dir <- "~/data/gome_2_sif_downscaled/data_orig/"
fileprefix <- "GOME_PK_dcSIF_005deg_8day_"
nclist <- paste0(dir, list.files(dir, pattern = paste0(fileprefix, ".*.nc"), recursive = TRUE))
outdir <- "~/data/gome_2_sif_downscaled/data_tidy/"
varnam <- "SIF"
lonnam <- "lon"
latnam <- "lat"
timenam <- "time"
timedimnam <- "time"

# ## get all available cores
# ncores <- parallel::detectCores()

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

## create files for each longitude slice, containing full time series wrapped for each gridcell (latitude)
nclist_to_df(
  nclist, 
	outdir = outdir, 
	fileprefix = fileprefix, 
	varnam = varnam, 
	ilon = irow_chunk[[as.integer(args[1])]],
	lonnam = lonnam, 
	latnam = latnam, 
	timenam = timenam, 
	timedimnam = timedimnam, 
	ncores = "all", 
	single_basedate = TRUE
	)
