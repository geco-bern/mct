library(dplyr)
library(rbeni)
library(tidyr)
library(purrr)
library(ncdf4)
library(lubridate)
library(extRemes)

source("R/mct.R")
source("R/get_plantwhc_mct_bysite.R")
source("R/get_plantwhc_mct_global.R")

## Invoke all at once.
dir <- "/alphadata01/bstocker/sofun/output_nc_global_sofun/"
df <- get_df_landmask(dir)
save(df, file = "./data/df_grid.Rdata")
df_test <- get_plantwhc_mct_global(df, dir) %>% 
  unnest(out_ilon_ilat)