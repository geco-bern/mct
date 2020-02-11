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
library(ingestr)

source("R/get_data_mct_global.R")
source("R/extract_points_filelist.R")
source("R/convert_et.R")
source("R/align_events.R")

## get sites for evaluation
# mysites <- rsofun::metainfo_Tier1_sites_kgclimate_fluxnet2015 %>% 
#   filter(!(classid %in% c("CRO", "WET"))) %>% 
#   filter(year_start<=2007 & year_end>=2007) %>%    # xxx test
#   pull(sitename)

siteinfo <- read_csv("~/data/FLUXNET-2015_Tier1/siteinfo_fluxnet2015_sofun+whc.csv") %>% 
  rename(sitename = mysitename) %>% 
  filter(!(classid %in% c("CRO", "WET"))) %>% 
  filter(year_start<=2007 & year_end>=2007) %>%    # xxx test
  mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>% 
  mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))

df_grid <- siteinfo %>% 
  select(sitename, lon, lat, elv) %>% 
  rename(idx = sitename)

##------------------------------------------------------------------------
## Get data from global fields (WATCH-WFDEI and LandFlux)
##------------------------------------------------------------------------
## df_grid must contain columns lon, lat, elv, and idx 

## PT-JPL
filn <- "data/df_pt_jpl.Rdata"
if (!file.exists(filn)){
  df_pt_jpl <- get_data_mct_global(
    df_grid, 
    dir_et = "~/data/landflux/et_prod/",    fil_et_pattern = "ET_PT-SRB-PU_daily_", 
    dir_prec = "~/data/watch_wfdei/Rainf_daily/", fil_prec_pattern = "Rainf_daily_WFDEI_CRU", 
    dir_snow = "~/data/watch_wfdei/Snowf_daily/", fil_snow_pattern = "Snowf_daily_WFDEI_CRU",
    dir_temp = "~/data/watch_wfdei/Tair_daily/",  fil_temp_pattern = "Tair_daily_WFDEI"
    )
  save(df_pt_jpl, file = filn)
} else {
  load(filn)
}

## PM
filn <- "data/df_pm_mod.Rdata"
if (!file.exists(filn)){
  df_pm_mod <- get_data_mct_global(
    df_grid, 
    dir_et = "~/data/landflux/et_prod/", fil_et_pattern = "ET_PM-SRB-PU_daily_", 
    dir_prec = "~/data/watch_wfdei/Rainf_daily/", fil_prec_pattern = "Rainf_daily_WFDEI_CRU", 
    dir_snow = "~/data/watch_wfdei/Snowf_daily/", fil_snow_pattern = "Snowf_daily_WFDEI_CRU",
    dir_temp = "~/data/watch_wfdei/Tair_daily/", fil_temp_pattern = "Tair_daily_WFDEI"
    )
  save(df_pm_mod, file = filn)
} else {
  load(filn)
}

## SEBS
filn <- "data/df_sebs.Rdata"
if (!file.exists(filn)){
  df_sebs <- get_data_mct_global(
    df_grid, 
    dir_et = "~/data/landflux/et_prod/", fil_et_pattern = "ET_SEBS-SRB-PU_daily_", 
    dir_prec = "~/data/watch_wfdei/Rainf_daily/", fil_prec_pattern = "Rainf_daily_WFDEI_CRU", 
    dir_snow = "~/data/watch_wfdei/Snowf_daily/", fil_snow_pattern = "Snowf_daily_WFDEI_CRU",
    dir_temp = "~/data/watch_wfdei/Tair_daily/", fil_temp_pattern = "Tair_daily_WFDEI"
    )
  save(df_sebs, file = filn)
} else {
  load(filn)
}

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
## meteo forcing data
ddf_meteo <- ingest(
  siteinfo = siteinfo,
  source    = "fluxnet2015", 
  getvars   = list(temp = "TA_F_DAY", prec = "P_F", vpd  = "VPD_F_DAY", swin =  "SW_IN_F", netrad = "NETRAD", patm = "PA_F"),
  dir       = "~/data/FLUXNET-2015_Tier1/20191024/DD/",
  settings  = list(dir_hh = "~/data/FLUXNET-2015_Tier1/20191024/HH/", getswc = FALSE),
  timescale = "d"
  )

## get data for idfferent time scales separately
filn <- "data/ddf_eval.Rdata"
if (!file.exists(filn)){
  ddf_eval <- ingest(
    siteinfo = siteinfo,
    source    = "fluxnet2015", 
    getvars   = list(latenth = "LE_F_MDS", latenth_qc = "LE_F_MDS_QC"),
    dir       = "~/data/FLUXNET-2015_Tier1/20191024/DD/",
    settings  = list(threshold_LE = 0.8, getswc = TRUE),
    timescale = "d"
    )
  save(ddf_eval, file = filn)
} else {
  load(filn)
}

## test plot
ggplot() +
  geom_line(data = ddf_eval %>% filter(sitename == "FR-Pue" & year(date)==2007), aes(x = date, y = latenth)) +
  geom_line(data = df_pt_jpl$df[[which(df_pt_jpl$idx == "FR-Pue")]], aes(x = date, y = et), col = "springgreen4") +
  geom_line(data = df_sebs$df[[which(df_sebs$idx == "FR-Pue")]], aes(x = date, y = et), col = "springgreen3")

filn <- "data/mdf_eval.Rdata"
if (!file.exists(filn)){
  mdf_eval <- ingest(
    siteinfo = siteinfo,
    source    = "fluxnet2015", 
    getvars   = list(latenth = "LE_F_MDS", latenth_qc = "LE_F_MDS_QC"),
    dir       = "~/data/FLUXNET-2015_Tier1/20191024/MM/",
    settings  = list(threshold_LE = 0.8, getswc = FALSE),
    timescale = "m"
    ) %>% 
    tidyr::drop_na(latenth)
  save(mdf_eval, file = filn)
} else {
  load(filn)
}

filn <- "data/adf_eval.Rdata"
if (!file.exists(filn)){
  adf_eval <- ingest(
    siteinfo = siteinfo,
    source    = "fluxnet2015", 
    getvars   = list(latenth = "LE_F_MDS", latenth_qc = "LE_F_MDS_QC"),
    dir       = "~/data/FLUXNET-2015_Tier1/20191024/YY/",
    settings  = list(threshold_LE = 0.8, getswc = FALSE),
    timescale = "y"
    ) %>% 
    tidyr::drop_na(latenth)
  save(adf_eval, file = filn)
} else {
  load(filn)
}

settings_eval <- list(
  benchmark = list( latenth = c("fluxnet2015") )
  )
obs_eval <- get_obs_eval2( settings_eval = settings_eval, adf = adf_eval, mdf = mdf_eval, ddf = ddf_eval )

# settings_eval <- list(
#   sitenames           = settings_sims$sitename,
#   sitenames_siteplots = mysites,
#   agg                 = 8,
#   path_fluxnet2015_d  = "~/data/FLUXNET-2015_Tier1/20191024/DD/",
#   path_fluxnet2015_w  = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_7d/original/unpacked/",
#   path_fluxnet2015_m  = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_1m/original/unpacked/",
#   path_fluxnet2015_y  = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_1y/original/unpacked/",
#   benchmark           = list( latenth = c("fluxnet2015") ),
#   remove_premodis     = TRUE
# )


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

save(out_eval_sebs, file = "./data/out_eval_sebs.Rdata")

## some test plots
out_eval_sebs$latenth$fluxnet2015$data$ddf %>% 
  filter(sitename == "FR-Pue" & lubridate::year(date) == 2007) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = obs)) +
  geom_line(aes(y = mod), col = "red")

out_eval_pt_jpl$latenth$fluxnet2015$data$ddf %>% 
  filter(sitename == "FR-Pue" & lubridate::year(date) == 2007) %>% 
  filter(sitename == "FR-Pue") %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = obs)) +
  geom_line(aes(y = mod), col = "red")

out_modobs_sebs <- out_eval_sebs$latenth$fluxnet2015$data$ddf %>%
  analyse_modobs2("mod", "obs", type = "heat")
out_modobs_sebs$gg

out_modobs_pt_jpl <- out_eval_pt_jpl$latenth$fluxnet2015$data$ddf %>%
  analyse_modobs2("mod", "obs", type = "heat")
out_modobs_pt_jpl$gg

##------------------------------------------------------------------------
## Align along rain-free periods of >= 14 days
##------------------------------------------------------------------------
## First, combine FLUXNET and LandFlux data to big flat table
df_eval <- ddf_meteo %>%
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

