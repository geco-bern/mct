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
library(rsofun)

source("R/get_data_mct_global.R")
source("R/extract_points_filelist.R")
source("R/convert_et.R")
source("R/align_events.R")

## get sites for evaluation
mysites <- rsofun::metainfo_Tier1_sites_kgclimate_fluxnet2015 %>% 
  filter(!(classid %in% c("CRO", "WET"))) %>% 
  filter(year_start<=2007 & year_end>=2007) %>%    # xxx test
  pull(sitename)

df_grid <- rsofun::metainfo_Tier1_sites_kgclimate_fluxnet2015 %>% 
  filter(sitename %in% mysites) %>% 
  select(sitename, lon, lat, elv) %>% 
  rename(idx = sitename)

##------------------------------------------------------------------------
## Get data from global fields (WATCH-WFDEI and LandFlux)
##------------------------------------------------------------------------
## df_grid must contain columns lon, lat, elv, and idx 

## PT-JPL
df_pt_jpl <- get_data_mct_global(
  df_grid, 
  dir_et = "~/data/landflux/et_prod/",    fil_et_pattern = "ET_PT-SRB-PU_daily_", 
  dir_prec = "~/data/watch_wfdei/Rainf_daily/", fil_prec_pattern = "Rainf_daily_WFDEI_CRU", 
  dir_snow = "~/data/watch_wfdei/Snowf_daily/", fil_snow_pattern = "Snowf_daily_WFDEI_CRU",
  dir_temp = "~/data/watch_wfdei/Tair_daily/",  fil_temp_pattern = "Tair_daily_WFDEI"
  )
save(df_pt_jpl, file = "df_pt_jpl.Rdata")

## PM
df_pm_mod <- get_data_mct_global(
  df_grid, 
  dir_et = "~/data/landflux/et_prod/", fil_et_pattern = "ET_PM-SRB-PU_daily_", 
  dir_prec = "~/data/watch_wfdei/Rainf_daily/", fil_prec_pattern = "Rainf_daily_WFDEI_CRU", 
  dir_snow = "~/data/watch_wfdei/Snowf_daily/", fil_snow_pattern = "Snowf_daily_WFDEI_CRU",
  dir_temp = "~/data/watch_wfdei/Tair_daily/", fil_temp_pattern = "Tair_daily_WFDEI"
  )
save(df_pm_mod, file = "data/df_pm_mod.Rdata")

## SEBS
df_sebs <- get_data_mct_global(
  df_grid, 
  dir_et = "~/data/landflux/et_prod/", fil_et_pattern = "ET_SEBS-SRB-PU_daily_", 
  dir_prec = "~/data/watch_wfdei/Rainf_daily/", fil_prec_pattern = "Rainf_daily_WFDEI_CRU", 
  dir_snow = "~/data/watch_wfdei/Snowf_daily/", fil_snow_pattern = "Snowf_daily_WFDEI_CRU",
  dir_temp = "~/data/watch_wfdei/Tair_daily/", fil_temp_pattern = "Tair_daily_WFDEI"
  )
save(df_sebs, file = "data/df_sebs.Rdata")

# ## example plots for one site
# ggplot() +
#   geom_line(data = df_pt_jpl$df[[which(df_pt_jpl$idx == "FR-Pue")]], aes(x = date, y = transp_mm), col = "springgreen4") +
#   geom_line(data = df_pt_jpl$df[[which(df_pt_jpl$idx == "FR-Pue")]], aes(x = date, y = evap_canop_mm), col = "blue") +
#   geom_line(data = df_pt_jpl$df[[which(df_pt_jpl$idx == "FR-Pue")]], aes(x = date, y = evap_soil_mm), col = "brown")
# 
# ggplot() +
#   geom_line(data = df_pt_jpl$df[[which(df_pt_jpl$idx == "FR-Pue")]], aes(x = date, y = transp_mm), col = "springgreen4") +
#   geom_line(data = df_sebs$df[[which(df_sebs$idx == "FR-Pue")]], aes(x = date, y = transp_mm), col = "springgreen3")

##------------------------------------------------------------------------
## Get data from FLUXNET2015 using rsofun
##------------------------------------------------------------------------
path_siteinfo <- "./siteinfo_fluxnet2015.csv"
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
  path_fluxnet2015_d  = "~/data/FLUXNET-2015_Tier1/20191024/DD/",
  path_fluxnet2015_w  = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_7d/original/unpacked/",
  path_fluxnet2015_m  = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_1m/original/unpacked/",
  path_fluxnet2015_y  = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_1y/original/unpacked/",
  benchmark           = list( latenth = c("fluxnet2015") ),
  remove_premodis     = TRUE
)

obs_eval  <- get_obs_eval( 
  settings_eval = settings_eval, 
  settings_sims = settings_sims, 
  overwrite     = TRUE, 
  light         = TRUE,
  add_forcing   = FALSE
)

##------------------------------------------------------------------------
## Evaluate data using rsofun
##------------------------------------------------------------------------
## PT-JPL
mod_pt_jpl <- df_pt_jpl %>% 
  unnest(df) %>% 
  select(sitename = idx, date, latenth = et)

out_eval_pt_jpl <- eval_sofun( 
  mod_pt_jpl, 
  settings_eval, 
  settings_sims, 
  obs_eval = obs_eval, 
  overwrite = TRUE, 
  light = TRUE )

# out_eval_pt_jpl$latenth$fluxnet2015$data$ddf %>% 
#   analyse_modobs2("mod", "obs", type = "heat")

save(out_eval_pt_jpl, file = "data/out_eval_pt_jpl.Rdata")


## SEBS
mod_sebs <- df_sebs %>% 
  unnest(df) %>% 
  select(sitename = idx, date, latenth = et)

out_eval_sebs <- eval_sofun( 
  mod_sebs, 
  settings_eval, 
  settings_sims, 
  obs_eval = obs_eval, 
  overwrite = TRUE, 
  light = TRUE )

# out_eval_sebs$latenth$fluxnet2015$data$ddf %>% 
#   analyse_modobs2("mod", "obs", type = "heat")

save(out_eval_sebs, file = "./data/out_eval_sebs.Rdata")

##------------------------------------------------------------------------
## Align along rain-free periods of >= 14 days
##------------------------------------------------------------------------
## First, combine FLUXNET and LandFlux data to big flat table
df_eval <- ddf_input %>% 
  select(sitename, date, temp_fluxnet = temp, prec_fluxnet = prec) %>% 
  left_join(
    obs_eval$ddf %>% 
      select(sitename, date, et_fluxnet = latenth),
    by = c("sitename", "date")
  ) %>% 
  left_join(
    df_pt_jpl %>% 
      unnest(df) %>% 
      select(sitename = idx, date, temp_watch = temp, prec_watch = prec, et_pt_jpl = et),
    by = c("sitename", "date")
  ) %>%
  left_join(
    df_pm_mod %>% 
      unnest(df) %>% 
      select(sitename = idx, date, et_pm_mod = et),
    by = c("sitename", "date")
  ) %>%
  left_join(
    df_sebs %>% 
      unnest(df) %>% 
      select(sitename = idx, date, et_sebs = et),
    by = c("sitename", "date")
  )

# ## xxx test
# df_eval <- df_eval %>% 
#   filter(year(date)==2007)

write_csv(df_eval, path = "./data/df_eval.csv")

## align data
df_alg <- df_eval %>%
  dplyr::mutate(et_bias_pt_jpl = et_pt_jpl - et_fluxnet) %>% 
  dplyr::mutate(et_bias_pm_mod = et_pm_mod - et_fluxnet) %>% 
  dplyr::mutate(et_bias_sebs   = et_sebs   - et_fluxnet) %>% 
  dplyr::rename(isevent = prec_fluxnet, site = sitename) %>% 
  align_events(
    dovars         = c("et_bias_pt_jpl", "et_bias_pm_mod", "et_bias_sebs"), 
    leng_threshold = 14, 
    before         = 10, 
    after          = 30, 
    nbins          = 4, 
    do_norm        = TRUE
    )

df_alg$df_dday_aggbydday %>% 
  ggplot(aes(x = dday)) + 
  geom_line(aes(y = et_bias_pt_jpl_median)) +
  geom_smooth(aes(y = et_bias_pt_jpl_median), color = 'red', method = 'loess', se = FALSE) +
  geom_ribbon(aes(ymin = et_bias_pt_jpl_q33, ymax = et_bias_pt_jpl_q66), fill = "black", alpha = 0.3) +
  labs(title = "PT-JPL")

df_alg$df_dday_aggbydday %>% 
  ggplot(aes(x = dday)) + 
  geom_ribbon(aes(ymin = et_bias_pm_mod_q33, ymax = et_bias_pm_mod_q66), fill = "black", alpha = 0.3) +
  geom_line(aes(y = et_bias_pm_mod_median)) +
  geom_smooth(aes(y = et_bias_pm_mod_median), color = 'red', method = 'loess', se = FALSE) +
  labs(title = "PM-MOD")

df_alg$df_dday_aggbydday %>% 
  ggplot(aes(x = dday)) + 
  geom_ribbon(aes(ymin = et_bias_sebs_q33, ymax = et_bias_sebs_q66), fill = "black", alpha = 0.3) +
  geom_line(aes(y = et_bias_sebs_median)) +
  geom_smooth(aes(y = et_bias_sebs_median), color = 'red', method = 'loess', se = FALSE) +
  labs(title = "SEBS")

save(df_alg, file = "data/df_alg.Rdata")
write_csv(df_alg$df_dday_aggbydday, path = "data/df_alg__df_dday_aggbydday.Rdata")

# ##------------------------------------------------------------------------
# ## Some test plots
# ##------------------------------------------------------------------------
# ## Compare temperature
# testsite <- mysites[51]
# df_eval %>% 
#   filter(sitename == testsite) %>% 
#   tidyr::gather("source", "temp", c(temp_watch, temp_fluxnet)) %>% 
#   ggplot(aes(x = date, y = temp, color = source)) + 
#     geom_line() +
#     labs(title = testsite)
# 
# ## Compare precipitation
# df_eval %>% 
#   filter(sitename == testsite) %>% 
#   tidyr::gather("source", "prec", c(prec_watch, prec_fluxnet)) %>% 
#   ggplot(aes(x = date, y = prec, color = source)) + 
#   geom_line() +
#   labs(title = testsite)
# 
# ## Compare ET PT-JPL
# df_eval %>% 
#   filter(sitename == testsite) %>% 
#   tidyr::gather("source", "et", c(et_pt_jpl, et_fluxnet)) %>% 
#   ggplot(aes(x = date, y = et, color = source)) + 
#   geom_line() +
#   labs(title = testsite)
# 
# df_eval %>% 
#   filter(sitename == testsite) %>% 
#   analyse_modobs2("et_pt_jpl", "et_fluxnet", type = "heat")
# 
# ## Compare ET PM-MOD
# df_eval %>% 
#   filter(sitename == testsite) %>% 
#   tidyr::gather("source", "et", c(et_pm_mod, et_fluxnet)) %>% 
#   ggplot(aes(x = date, y = et, color = source)) + 
#   geom_line() +
#   labs(title = testsite)
# 
# df_eval %>% 
#   filter(sitename == testsite) %>% 
#   analyse_modobs2("et_pm_mod", "et_fluxnet", type = "heat")
# 
# 
# ## Compare ET SEBS
# testsite <- mysites[8]
# df_eval %>% 
#   filter(sitename == testsite) %>% 
#   tidyr::gather("source", "et", c(et_sebs, et_fluxnet)) %>% 
#   ggplot(aes(x = date, y = et, color = source)) + 
#   geom_line() +
#   labs(title = testsite)
# 
# df_eval %>% 
#   filter(sitename == testsite) %>% 
#   analyse_modobs2("et_sebs", "et_fluxnet", type = "heat")

