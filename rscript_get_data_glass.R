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

## create files for each longitude slice, containing full time series wrapped for each gridcell (latitude)
rbeni::nclist_to_df(nclist, outdir, fileprefix, varnam, lonnam, latnam, timenam, timedimnam, ncores = 160, single_basedate = FALSE, fgetdate = fgetdate_glass)
