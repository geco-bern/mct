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
df <- get_df_landmask(dir) %>% 
  get_plantwhc_mct_global(dir) %>% 
  unnest(out_ilon_ilat)

save(df, file="./data/df_plantwhc_mct.Rdata")
