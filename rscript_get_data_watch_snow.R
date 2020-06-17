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
fileprefix <- "Snowf_daily_WFDEI_CRU_"
dir <- "~/data/watch_wfdei/"
nclist <- paste0(dir, list.files(dir, pattern = paste0(fileprefix, ".*.nc"), recursive = TRUE))
outdir <- "~/data/watch_wfdei/data_tidy/"
varnam <- "Snowf"
lonnam <- "lon"
timenam <- "timestp"
timedimnam <- "tstep"

## create files for each longitude slice, containing full time series wrapped for each gridcell (latitude)
rbeni::nclist_to_df(nclist, outdir, fileprefix, varnam, lonnam, timenam, timedimnam, ncores = 32, single_basedate = FALSE)
