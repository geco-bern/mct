library(dplyr)
library(rbeni)
library(tidyr)
library(purrr)
library(ncdf4)
library(lubridate)
library(extRemes)
library(R.utils)
library(ggplot2)
library(readr)

source("R/mct.R")
source("R/get_plantwhc_mct_bysite.R")
source("R/get_plantwhc_mct_global.R")
source('~/mct/R/extract_points_filelist.R')
source("R/get_df_landmask.R")
source("R/convert_et.R")

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

## xxx test
mysites <- rsofun::metainfo_Tier1_sites_kgclimate_fluxnet2015 %>% 
  filter(year_start<=2005, year_end>=2005) %>% 
  filter(!(classid %in% c("CRO", "WET"))) %>% 
  pull(sitename)

df_grid <- rsofun::metainfo_Tier1_sites_kgclimate_fluxnet2015 %>% 
  filter(sitename %in% mysites) %>% 
  select(sitename, lon, lat, elv) %>% 
  mutate(idx = 1:n())

## df_grid must contain columns lon, lat, elv, and idx 
#df <- get_plantwhc_mct_global(df_grid, dir, fapar_source = "fAPAR3g_meandoy")
df <- get_plantwhc_mct_global(df_grid, 
                              dir_et = "~/data/landflux/et_prod/", fil_et_pattern = "ET_PT-SRB-PU_daily_", varnam_et = "ET_tran", 
                              dir_prec = "~/data/watch_wfdei/Rainf_daily/", fil_prec_pattern = "Rainf_daily_WFDEI_CRU", varnam_prec = "Rainf", 
                              dir_snow = "~/data/watch_wfdei/Snowf_daily/", fil_snow_pattern = "Snowf_daily_WFDEI_CRU", varnam_snow = "Snowf",
                              dir_temp = "~/data/watch_wfdei/Tair_daily/", fil_temp_pattern = "Tair_daily_WFDEI", varnam_temp = "Tair")

##------------------------
## xxx test: comparison to tower data
##------------------------
path_siteinfo <- "~/siteinfo_fluxnet2015.csv"
siteinfo <- rsofun::metainfo_Tier1_sites_kgclimate_fluxnet2015 %>% 
  dplyr::filter(sitename %in% mysites) %>%
  write_csv(path = path_siteinfo)

settings_input <-  list(
  data                     = NA,
  temperature              = "fluxnet2015",
  precipitation            = "fluxnet2015",
  vpd                      = "fluxnet2015",
  ppfd                     = "fluxnet2015",
  netrad                   = "fluxnet2015",  #  c("fluxnet2015", "watch_wfdei"),
  patm                     = "fluxnet2015",
  netrad                   = NA,
  cloudcover               = "cru",
  path_input               = "~/sofun_inputs/example/",
  path_watch_wfdei         = "~/data/watch_wfdei/",
  path_cru                 = "~/data/cru/ts_4.01/",
  path_MODIS_FPAR_MCD15A3H = "~/data/fluxnet_subsets/fapar_MODIS_FPAR_MCD15A3H_gee_MCD15A3H_fluxnet2015_gee_subset/",
  path_MODIS_EVI_MOD13Q1   = "~/data/fluxnet_subsets/fapar_MODIS_EVI_MOD13Q1_gee_MOD13Q1_fluxnet2015_gee_subset/",
  path_co2                 = "~/data/co2/cCO2_rcp85_const850-1765.csv",
  path_fluxnet2015         = "~/data/FLUXNET-2015_Tier1/20191024/DD/",
  path_fluxnet2015_hh      = "~/data/FLUXNET-2015_Tier1/20191024/HH/",
  get_from_remote          = FALSE,
  settings_gee             = get_settings_gee( 
    bundle      = "fpar", 
    python_path = "/Users/benjaminstocker/Library/Enthought/Canopy_64bit/User/bin/python",
    gee_path    = "~/gee_subset/gee_subset/"
  ),
  fapar = "MODIS_FPAR_MCD15A3H",
  splined_fapar = TRUE
)

params_siml <- list(
  spinup             = TRUE,
  spinupyears        = 10,
  recycle            = 1,
  soilmstress        = FALSE,
  tempstress         = FALSE,
  calc_aet_fapar_vpd = FALSE,
  in_ppfd            = TRUE,
  in_netrad          = FALSE,
  const_clim_year    = -9999,
  const_lu_year      = -9999,
  const_co2_year     = -9999,
  const_ndep_year    = -9999,
  const_nfert_year   = -9999,
  outdt              = 1,
  ltre               = FALSE,
  ltne               = FALSE,
  ltrd               = FALSE,
  ltnd               = FALSE,
  lgr3               = TRUE,
  lgn3               = FALSE,
  lgr4               = FALSE
)

settings_sims <- prepare_setup_sofun(siteinfo = siteinfo, params_siml = params_siml)

ddf_input <- prepare_input_sofun(
  settings_input             = settings_input,
  settings_sims              = settings_sims,
  overwrite_csv_climate_lev1 = FALSE,
  overwrite_csv_climate_lev2 = TRUE,
  overwrite_csv_climate_lev3 = TRUE,
  overwrite_rdata_climate    = TRUE,
  overwrite_csv_fapar        = FALSE,
  verbose                    = FALSE
)

settings_eval <- list(
  sitenames           = settings_sims$sitename,
  sitenames_siteplots = mysites,
  agg                 = 8,
  path_fluxnet2015_d  = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_1d/original/unpacked/",
  path_fluxnet2015_w  = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_7d/original/unpacked/",
  path_fluxnet2015_m  = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_1m/original/unpacked/",
  path_fluxnet2015_y  = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_1y/original/unpacked/",
  # path_gepisat_d      = "~/data/gepisat/v3_fluxnet2015/daily_gpp/",
  benchmark           = list( latenth = c("fluxnet2015_NT") ),
  remove_premodis     = TRUE
)

obs_eval  <- get_obs_eval( 
  settings_eval = settings_eval, 
  settings_sims = settings_sims, 
  overwrite     = TRUE, 
  light         = TRUE,
  add_forcing   = FALSE
)

df_test <- ddf_input %>% 
  select(sitename, date, temp_fluxnet = temp, prec_fluxnet = prec) %>% 
  filter(lubridate::year(date) == 2005) %>% 
  left_join(
    obs_eval$ddf %>% 
      filter(lubridate::year(date) == 2005) %>% 
      select(sitename, date, et_fluxnet = latenth),
    by = c("sitename", "date")
    ) %>% 
  left_join(
    df %>% 
      unnest(df) %>% 
      filter(lubridate::year(date) == 2005) %>% 
      select(sitename, date, temp_watch = temp, prec_watch = prec, et_landflux = et),
    by = c("sitename", "date")
  )


## Compare temperature
testsite <- mysites[3]
df_test %>% 
  filter(sitename == testsite) %>% 
  tidyr::gather("source", "temp", c(temp_watch, temp_fluxnet)) %>% 
  ggplot(aes(x = date, y = temp, color = source)) + 
    geom_line() +
    labs(title = testsite)

## Compare precipitation
df_test %>% 
  filter(sitename == testsite) %>% 
  tidyr::gather("source", "prec", c(prec_watch, prec_fluxnet)) %>% 
  ggplot(aes(x = date, y = prec, color = source)) + 
  geom_line() +
  labs(title = testsite)

## Compare ET
df_test %>% 
  filter(sitename == testsite) %>% 
  tidyr::gather("source", "et", c(et_landflux, et_fluxnet)) %>% 
  ggplot(aes(x = date, y = et, color = source)) + 
  geom_line() +
  labs(title = testsite)

