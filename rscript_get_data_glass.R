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
dir <- "~/data/glass/data_netcdf/"
fileprefix <- "GLASS07B01.V41."
nclist <- paste0(dir, list.files(dir, pattern = paste0(fileprefix, ".*.nc"), recursive = TRUE))
outdir <- "~/data/glass/data_tidy/"
varnam <- "NR"
lonnam <- "longitude"
latnam <- "latitude"
timenam <- "time"
timedimnam <- "time"

fgetdate_glass <- function(filnam){
  year <- stringr::str_sub(filnam, 47, 50)
  doy <- stringr::str_sub(filnam, 51, 53)
  date <- lubridate::ymd(paste0(year, "-01-01")) + lubridate::days(as.numeric(doy)) - lubridate::days(1)
  return(date)
}

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
rbeni::nclist_to_df(nclist, 
	outdir = outdir, 
	fileprefix = fileprefix, 
	varnam = varnam, 
	ilon = irow_chunk[[as.integer(args[1])]],
	lonnam = lonnam, 
	latnam = latnam, 
	timenam = timenam, 
	timedimnam = timedimnam, 
	ncores = "all", 
	single_basedate = FALSE, 
	fgetdate = fgetdate_glass
	)
