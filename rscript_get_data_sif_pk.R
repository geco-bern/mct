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

## create files for each longitude slice, containing full time series wrapped for each gridcell (latitude), function from rbeni package
nclist_to_df(nclist, outdir, fileprefix, varnam, ilon = NA, lonnam = lonnam, latnam = latnam, timenam = timenam, timedimnam = timedimnam, ncores = 1, single_basedate = TRUE)
