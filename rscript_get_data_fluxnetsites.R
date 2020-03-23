library(dplyr)
library(rbeni)
library(tidyr)
library(purrr)
library(ncdf4)
library(lubridate)
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
  # filter(year_start<=2007 & year_end>=2007) %>%    # xxx test
  mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>% 
  mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))

df_grid <- siteinfo %>% 
  dplyr::select(sitename, lon, lat, elv) %>% 
  dplyr::rename(idx = sitename)

##------------------------------------------------------------------------
## Get data from global fields (WATCH-WFDEI and LandFlux)
##------------------------------------------------------------------------
## df_grid must contain columns lon, lat, elv, and idx 

## PT-JPL
filn <- "data/df_pt_jpl.Rdata"
if (!file.exists(filn)){
  filn_csv <- str_replace(filn, "Rdata", "csv")
  if (!file.exists(filn)){
    df_pt_jpl <- get_data_mct_global(
      df_grid, 
      dir_et   = "~/data/landflux/et_prod/",        fil_et_pattern = "ET_PT-SRB-PU_daily_", 
      dir_prec = "~/data/watch_wfdei/Rainf_daily/", fil_prec_pattern = "Rainf_daily_WFDEI_CRU", 
      dir_snow = "~/data/watch_wfdei/Snowf_daily/", fil_snow_pattern = "Snowf_daily_WFDEI_CRU",
      dir_temp = "~/data/watch_wfdei/Tair_daily/",  fil_temp_pattern = "Tair_daily_WFDEI"
    )
    save(df_pt_jpl, file = filn)
  }
} else {
  load(filn)
}

## PM
filn <- "data/df_pm_mod.Rdata"
if (!file.exists(filn)){
  df_pm_mod <- get_data_mct_global(
    df_grid, 
    dir_et   = "~/data/landflux/et_prod/",        fil_et_pattern = "ET_PM-SRB-PU_daily_", 
    dir_prec = "~/data/watch_wfdei/Rainf_daily/", fil_prec_pattern = "Rainf_daily_WFDEI_CRU", 
    dir_snow = "~/data/watch_wfdei/Snowf_daily/", fil_snow_pattern = "Snowf_daily_WFDEI_CRU",
    dir_temp = "~/data/watch_wfdei/Tair_daily/",  fil_temp_pattern = "Tair_daily_WFDEI"
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
    dir_et   = "~/data/landflux/et_prod/",        fil_et_pattern = "ET_SEBS-SRB-PU_daily_", 
    dir_prec = "~/data/watch_wfdei/Rainf_daily/", fil_prec_pattern = "Rainf_daily_WFDEI_CRU", 
    dir_snow = "~/data/watch_wfdei/Snowf_daily/", fil_snow_pattern = "Snowf_daily_WFDEI_CRU",
    dir_temp = "~/data/watch_wfdei/Tair_daily/",  fil_temp_pattern = "Tair_daily_WFDEI"
    )
  save(df_sebs, file = filn)
} else {
  load(filn)
}