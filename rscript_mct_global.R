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

grid_nc_file <- "~/data/watch_wfdei/WFDEI-elevation.nc"
gridfile <- "./data/df_grid.Rdata"
if (file.exists(gridfile)){
  load(gridfile)
} else {
  df_grid <- get_df_landmask(grid_nc_file)
  save(df_grid, file = gridfile)
  
  ## test : yes, does the same
  rasta <- raster::raster(grid_nc_file)
  df_test <- raster::extract(rasta, sp::SpatialPoints(dplyr::select(df_grid, lon, lat)), sp = TRUE) %>% 
    as_tibble() %>% 
    rename(elv_test = elevation.above.sea.level.using.CRU.heights) %>% 
    left_join(df_grid, by = c("lon", "lat"))

}
#df <- get_plantwhc_mct_global(df_grid, dir, fapar_source = "fAPAR3g_meandoy")
df <- get_plantwhc_mct_global(df_grid, 
                              dir_et = "~/data/landflux/et_prod/", fil_et_pattern = "ET_PT-SRB-PU_daily_", varnam_et = "ET_tran", 
                              dir_prec = "~/data/watch_wfdei/Rainf_daily/", fil_prec_pattern = "Rainf_daily_WFDEI_CRU", varnam_prec = "Rainf", 
                              dir_snow = "~/data/watch_wfdei/Snowf_daily/", fil_snow_pattern = "Snowf_daily_WFDEI_CRU", varnam_snow = "Snowf",
                              dir_temp = "~/data/watch_wfdei/Tair_daily/", fil_temp_pattern = "Tair_daily_WFDEI", varnam_temp = "Tair")

