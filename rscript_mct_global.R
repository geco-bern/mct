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


dir <- "~/sofun/output_nc_global_sofun/"

df_grid <- get_df_landmask(dir)
save(df_grid, file = "./data/df_grid.Rdata")

df <- get_plantwhc_mct_global(df_grid, dir) %>% 
  unnest(out_ilon_ilat)

