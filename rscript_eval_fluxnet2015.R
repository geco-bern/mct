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

siteinfo <- read_csv("~/rsofun/siteinfo_fluxnet2015.csv") %>% 
  filter(!(classid %in% c("CRO", "WET"))) %>% 
  mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>% 
  mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))

df_grid <- siteinfo %>% 
  dplyr::select(sitename, lon, lat, elv) %>% 
  dplyr::rename(idx = sitename)

##------------------------------------------------------------------------
## Get data from global fields (WATCH-WFDEI and LandFlux)
##------------------------------------------------------------------------
## df_grid must contain columns lon, lat, elv, and idx 

## PT-JPL, covering 1984-2007
load("data/df_pt_jpl.Rdata")

## PM, covering 1984-2007
load("data/df_pm_mod.Rdata")

## SEBS, covering 1984-2007
load("data/df_sebs.Rdata")

## ALEXI, covering 2003-2018
load("data/df_alexi.Rdata")

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
  source    = "fluxnet",
  siteinfo  = siteinfo,
  getvars   = list(temp = "TA_F_DAY", prec = "P_F", vpd  = "VPD_F_DAY", swin =  "SW_IN_F", netrad = "NETRAD", patm = "PA_F"),
  dir       = "~/data/FLUXNET-2015_Tier1/20191024/DD/",
  settings  = list(dir_hh = "~/data/FLUXNET-2015_Tier1/20191024/HH/", getswc = FALSE),
  timescale = "d"
  )
save(ddf_meteo, file = "./data/ddf_meteo.Rdata")

## get data for idfferent time scales separately
filn <- "data/ddf_eval.Rdata"
if (!file.exists(filn)){
  ddf_eval <- ingest(
    siteinfo = siteinfo,
    source    = "fluxnet", 
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
  geom_line(data = ddf_eval %>% filter(sitename == "FR-Pue") %>% unnest(data), aes(x = date, y = latenth)) +
  geom_line(data = df_pt_jpl %>% filter(idx == "FR-Pue") %>% unnest(df), aes(x = date, y = et), col = "springgreen4") +
  geom_line(data = df_sebs %>% filter(idx == "FR-Pue") %>% unnest(df), aes(x = date, y = et), col = "springgreen3") +
  xlim(ymd("2004-01-01"), ymd("2007-01-01"))

filn <- "data/mdf_eval.Rdata"
if (!file.exists(filn)){
  mdf_eval <- ingest(
    siteinfo = siteinfo,
    source    = "fluxnet", 
    getvars   = list(latenth = "LE_F_MDS", latenth_qc = "LE_F_MDS_QC"),
    dir       = "~/data/FLUXNET-2015_Tier1/20191024/MM/",
    settings  = list(threshold_LE = 0.8, getswc = FALSE),
    timescale = "m"
    )
    # ) %>% 
    # tidyr::drop_na(latenth)
  save(mdf_eval, file = filn)
} else {
  load(filn)
}

filn <- "data/adf_eval.Rdata"
if (!file.exists(filn)){
  adf_eval <- ingest(
    siteinfo = siteinfo,
    source    = "fluxnet", 
    getvars   = list(latenth = "LE_F_MDS", latenth_qc = "LE_F_MDS_QC"),
    dir       = "~/data/FLUXNET-2015_Tier1/20191024/YY/",
    settings  = list(threshold_LE = 0.8, getswc = FALSE),
    timescale = "y"
    ) 
    # tidyr::drop_na(latenth)
  save(adf_eval, file = filn)
} else {
  load(filn)
}

settings_eval <- list(
  benchmark = list( latenth = c("fluxnet") ),
  sitenames = siteinfo %>% pull(sitename),
  agg       = 8  # An integer specifying the number of days used to define the width of bins for daily data aggregated to several days
)
obs_eval <- collect_obs_eval( 
  siteinfo = siteinfo,
  settings = settings_eval, 
  adf = adf_eval, 
  mdf = mdf_eval, 
  ddf = ddf_eval 
)

##------------------------------------------------------------------------
## Evaluate data using rsofun
##------------------------------------------------------------------------
## PT-JPL
mod_pt_jpl <- df_pt_jpl %>% 
  rename(data = df, sitename = idx) %>% 
  mutate(data = purrr::map(data, ~rename(., latenth = et)))

out_eval_pt_jpl <- eval_sofun( 
  mod_pt_jpl, 
  settings_eval, 
  obs_eval = obs_eval, 
  overwrite = TRUE, 
  light = TRUE )

# ## use format as described in rsofun/vignettes/example.Rmd
# out_eval <- eval_sofun( 
#   df_output, 
#   settings_eval, 
#   settings_sims, 
#   obs_eval = obs_eval, 
#   overwrite = TRUE, 
#   light = FALSE 
# )

# out_eval_pt_jpl$latenth$fluxnet$data$ddf %>% 
#   analyse_modobs2("mod", "obs", type = "heat")

save(out_eval_pt_jpl, file = "data/out_eval_pt_jpl.Rdata")

## PM-MOD
mod_pm_mod <- df_pm_mod %>% 
  rename(data = df, sitename = idx) %>% 
  mutate(data = purrr::map(data, ~rename(., latenth = et)))

out_eval_pm_mod <- eval_sofun( 
  mod_pm_mod, 
  settings_eval, 
  obs_eval = obs_eval, 
  overwrite = TRUE, 
  light = TRUE )

save(out_eval_pm_mod, file = "./data/out_eval_pm_mod.Rdata")


## SEBS
mod_sebs <- df_sebs %>% 
  rename(data = df, sitename = idx) %>% 
  mutate(data = purrr::map(data, ~rename(., latenth = et)))

out_eval_sebs <- eval_sofun( 
  mod_sebs, 
  settings_eval, 
  obs_eval = obs_eval, 
  overwrite = TRUE, 
  light = TRUE )

save(out_eval_sebs, file = "./data/out_eval_sebs.Rdata")


## ALEXI
mod_alexi <- df_alexi %>% 
  rename(data = df, sitename = idx) %>% 
  mutate(data = purrr::map(data, ~rename(., latenth = et)))

out_eval_alexi <- eval_sofun( 
  mod_alexi, 
  settings_eval, 
  obs_eval = obs_eval, 
  overwrite = TRUE, 
  light = TRUE )

save(out_eval_alexi, file = "./data/out_eval_alexi.Rdata")


## some test plots
out_eval_sebs$latenth$fluxnet$data$ddf %>% 
  dplyr::filter(sitename == "FR-Pue" & lubridate::year(date) == 2007) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = obs)) +
  geom_line(aes(y = mod), col = "red")

out_eval_pt_jpl$latenth$fluxnet$data$ddf %>% 
  filter(sitename == "FR-Pue" & lubridate::year(date) == 2007) %>% 
  filter(sitename == "FR-Pue") %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = obs)) +
  geom_line(aes(y = mod), col = "red")

out_eval_pm_mod$latenth$fluxnet$data$ddf %>% 
  filter(sitename == "FR-Pue" & lubridate::year(date) == 2007) %>% 
  filter(sitename == "FR-Pue") %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = obs)) +
  geom_line(aes(y = mod), col = "red")

out_eval_alexi$latenth$fluxnet$data$ddf %>% 
  filter(sitename == "FR-Pue" & lubridate::year(date) == 2007) %>% 
  filter(sitename == "FR-Pue") %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = obs)) +
  geom_line(aes(y = mod), col = "red")


out_modobs_sebs <- out_eval_sebs$latenth$fluxnet$data$ddf %>%
  analyse_modobs2("mod", "obs", type = "heat")
out_modobs_sebs$gg

out_modobs_pt_jpl <- out_eval_pt_jpl$latenth$fluxnet$data$ddf %>%
  analyse_modobs2("mod", "obs", type = "heat")
out_modobs_pt_jpl$gg

out_modobs_pm_mod <- out_eval_pm_mod$latenth$fluxnet$data$ddf %>%
  analyse_modobs2("mod", "obs", type = "heat")
out_modobs_pm_mod$gg

out_modobs_alexi <- out_eval_alexi$latenth$fluxnet$data$ddf %>%
  analyse_modobs2("mod", "obs", type = "heat")
out_modobs_alexi$gg

##------------------------------------------------------------------------
## Align along rain-free periods of >= 14 days
##------------------------------------------------------------------------
## First, combine FLUXNET and LandFlux data to big flat table
convert_to_mega <- function(x){x * 1e-6}
df_eval <- ddf_meteo %>%
  tidyr::unnest(data) %>% 
  dplyr::select(sitename, date, temp_fluxnet = temp, prec_fluxnet = prec) %>%
  full_join(
    obs_eval$ddf %>%
      tidyr::unnest(data) %>% 
      dplyr::select(sitename, date, et_fluxnet = latenth),
    by = c("sitename", "date")
  ) %>%
  full_join(
    df_pt_jpl %>% 
      unnest(df) %>% 
      dplyr::select(sitename = idx, date, temp_watch = temp, prec_watch = prec, et_pt_jpl = et),
    by = c("sitename", "date")
  ) %>%
  full_join(
    df_pm_mod %>% 
      unnest(df) %>% 
      dplyr::select(sitename = idx, date, et_pm_mod = et),
    by = c("sitename", "date")
  ) %>%
  full_join(
    df_sebs %>% 
      unnest(df) %>% 
      dplyr::select(sitename = idx, date, et_sebs = et),
    by = c("sitename", "date")
  ) %>%
  full_join(
    df_alexi %>% 
      unnest(df) %>% 
      dplyr::select(sitename = idx, date, et_alexi = et),
    by = c("sitename", "date")
  ) %>% 
  mutate_at(vars(starts_with("et_")), list(convert_to_mega))

# ## xxx test
# df_eval <- df_eval %>%
#   filter(year(date)==2007)

write_csv(df_eval, path = "./data/df_eval.csv")

## align data
df_alg <- df_eval %>%
  dplyr::mutate(et_bias_pt_jpl = et_pt_jpl - et_fluxnet) %>% 
  dplyr::mutate(et_bias_pm_mod = et_pm_mod - et_fluxnet) %>% 
  dplyr::mutate(et_bias_sebs   = et_sebs   - et_fluxnet) %>% 
  dplyr::mutate(et_bias_alexi  = et_alexi   - et_fluxnet) %>% 
  dplyr::rename(isevent = prec_fluxnet, site = sitename) %>% 
  dplyr::ungroup() %>% 
  align_events(
    dovars         = c("et_bias_pt_jpl", "et_bias_pm_mod", "et_bias_sebs", "et_bias_alexi"), 
    leng_threshold = 14, 
    before         = 10, 
    after          = 30, 
    nbins          = 4, 
    do_norm        = TRUE
    )

gg_alg_pt_jpl <- df_alg$df_dday_aggbydday %>% 
  ggplot(aes(x = dday)) + 
  geom_ribbon(aes(ymin = et_bias_pt_jpl_q33, ymax = et_bias_pt_jpl_q66), fill = "black", alpha = 0.3) +
  geom_line(aes(y = et_bias_pt_jpl_median)) +
  geom_smooth(aes(y = et_bias_pt_jpl_median), color = 'red', method = 'loess', se = FALSE) +
  labs(title = "PT-JPL", y = expression(paste("ET bias (MJ m"^{-2}, " d"^{-1},")")))

gg_alg_pm <- df_alg$df_dday_aggbydday %>% 
  ggplot(aes(x = dday)) + 
  geom_ribbon(aes(ymin = et_bias_pm_mod_q33, ymax = et_bias_pm_mod_q66), fill = "black", alpha = 0.3) +
  geom_line(aes(y = et_bias_pm_mod_median)) +
  geom_smooth(aes(y = et_bias_pm_mod_median), color = 'red', method = 'loess', se = FALSE) +
  labs(title = "PM-MOD", y = expression(paste("ET bias (MJ m"^{-2}, " d"^{-1},")")))

gg_alg_sebs <- df_alg$df_dday_aggbydday %>% 
  ggplot(aes(x = dday)) + 
  geom_ribbon(aes(ymin = et_bias_sebs_q33, ymax = et_bias_sebs_q66), fill = "black", alpha = 0.3) +
  geom_line(aes(y = et_bias_sebs_median)) +
  geom_smooth(aes(y = et_bias_sebs_median), color = 'red', method = 'loess', se = FALSE) +
  labs(title = "SEBS", y = expression(paste("ET bias (MJ m"^{-2}, " d"^{-1},")")))

gg_alg_alexi <- df_alg$df_dday_aggbydday %>% 
  ggplot(aes(x = dday)) + 
  geom_ribbon(aes(ymin = et_bias_alexi_q33, ymax = et_bias_alexi_q66), fill = "black", alpha = 0.3) +
  geom_line(aes(y = et_bias_alexi_median)) +
  geom_smooth(aes(y = et_bias_alexi_median), color = 'red', method = 'loess', se = FALSE) +
  labs(title = "ALEXI", y = expression(paste("ET bias (MJ m"^{-2}, " d"^{-1},")")))

library(patchwork)
gg_alg_pt_jpl + gg_alg_pm + gg_alg_sebs + gg_alg_alexi + plot_layout(nrow = 2)
ggsave("fig/et_bias_droughtday.pdf", width = 7, height = 6)

save(df_alg, file = "data/df_alg.Rdata")
write_csv(df_alg$df_dday_aggbydday, path = "data/df_alg__df_dday_aggbydday.Rdata")

##------------------------------------------------------------------------
## Some test plots
##------------------------------------------------------------------------
## Compare temperature
testsite <- "FR-Pue"
df_eval %>%
  filter(sitename == testsite) %>%
  tidyr::gather("source", "temp", c(temp_watch, temp_fluxnet)) %>%
  ggplot(aes(x = date, y = temp, color = source)) +
    geom_line() +
    labs(title = testsite)

## Compare precipitation
df_eval %>%
  filter(sitename == testsite) %>%
  tidyr::gather("source", "prec", c(prec_watch, prec_fluxnet)) %>%
  ggplot(aes(x = date, y = prec, color = source)) +
  geom_line() +
  labs(title = testsite)

## Compare ET PT-JPL
df_eval %>%
  filter(sitename == testsite) %>%
  tidyr::gather("source", "et", c(et_pt_jpl, et_fluxnet)) %>%
  ggplot(aes(x = date, y = et, color = source)) +
  geom_line() +
  labs(title = testsite)

df_eval %>%
  filter(sitename == testsite) %>%
  analyse_modobs2("et_pt_jpl", "et_fluxnet", type = "heat")

## Compare ET PM-MOD
df_eval %>%
  filter(sitename == testsite) %>%
  tidyr::gather("source", "et", c(et_pm_mod, et_fluxnet)) %>%
  ggplot(aes(x = date, y = et, color = source)) +
  geom_line() +
  labs(title = testsite)

df_eval %>%
  filter(sitename == testsite) %>%
  analyse_modobs2("et_pm_mod", "et_fluxnet", type = "heat")


## Compare ET SEBS
df_eval %>%
  filter(sitename == testsite) %>%
  tidyr::gather("source", "et", c(et_sebs, et_fluxnet)) %>%
  ggplot(aes(x = date, y = et, color = source)) +
  geom_line() +
  labs(title = testsite)

df_eval %>%
  filter(sitename == testsite) %>%
  analyse_modobs2("et_sebs", "et_fluxnet", type = "heat")

df_eval %>%
  analyse_modobs2("et_alexi", "et_fluxnet", type = "heat")

