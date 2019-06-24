library(dplyr)
library(rbeni)
library(tidyr)
library(purrr)
library(ncdf4)
library(lubridate)
library(extRemes)
library(R.utils)

source("R/mct.R")
source("R/get_plantwhc_mct_bysite.R")
source("R/get_plantwhc_mct_global.R")

dir <- "~/sofun/output_nc_global_sofun/"
gridfile <- "./data/df_grid.Rdata"
if (file.exists(gridfile)){
  load(gridfile)
} else {
  df_grid <- get_df_landmask(dir)
  save(df_grid, file = gridfile)
}
df <- get_plantwhc_mct_global(df_grid, dir)

