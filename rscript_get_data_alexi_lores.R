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
dir <- "~/data/alexi_tir/data_halfdeg/"
nclist <- paste0(dir, list.files(dir, pattern = "EDAY_CERES_"))
outdir <- "~/data/alexi_tir/data_tidy_halfdeg/"
varnam <- "et"
lonnam <- "lon"
fileprefix <- "EDAY_CERES_"

## create files for each longitude slice, containing full time series wrapped for each gridcell (latitude)
rbeni::nclist_to_df(nclist, outdir, fileprefix, varnam, lonnam, timenam = "time", ncores = 80) # 2400  ncores = nnodes * 16; nnodes is requested in submission file 
