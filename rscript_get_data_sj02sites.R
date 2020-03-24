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
library(stringr)

source("R/get_data_mct_global.R")
source("R/extract_points_filelist.R")
source("R/convert_et.R")
source("R/align_events.R")

## Get meta info of sites (lon, lat)
siteinfo <- read_csv("~/data/rootingdepth/root_profiles_schenkjackson02/data/root_profiles_D50D95.csv") %>%
  dplyr::filter(Wetland == "N" & Anthropogenic == "N" & Schenk_Jackson_2002 == "YES") %>% 
  dplyr::rename(sitename = ID, lat = Latitude, lon = Longitude) %>% 
  dplyr::mutate(elv = ifelse(elv==-999, NA, elv)) %>% 
  dplyr::filter(lon!=-999 & lat!=-999) %>% 
  dplyr::mutate(year_start = 1982, year_end = 2011) %>% 
  dplyr::select(sitename, lon, lat, elv, year_start, year_end)

df_grid <- siteinfo %>% 
  dplyr::select(sitename, lon, lat, elv) %>% 
  dplyr::rename(idx = sitename)

##------------------------------------------------------------------------
## Get data from global fields (WATCH-WFDEI and LandFlux)
##------------------------------------------------------------------------
## df_grid must contain columns lon, lat, elv, and idx

## PT-JPL
filn <- "data/df_pt_jpl_sj02.Rdata"
filn_csv <- str_replace(filn, "Rdata", "csv")
if (!file.exists(filn)){
  if (!file.exists(filn_csv)){
    df_pt_jpl <- get_data_mct_global(
      df_grid,
      dir_et   = "~/data/landflux/et_prod/",        fil_et_pattern   = "ET_PT-SRB-PU_daily_",
      dir_prec = "~/data/watch_wfdei/Rainf_daily/", fil_prec_pattern = "Rainf_daily_WFDEI_CRU",
      dir_snow = "~/data/watch_wfdei/Snowf_daily/", fil_snow_pattern = "Snowf_daily_WFDEI_CRU",
      dir_temp = "~/data/watch_wfdei/Tair_daily/",  fil_temp_pattern = "Tair_daily_WFDEI",
      get_watch = TRUE, get_landeval = TRUE, get_alexi = FALSE,
      year_start_watch = 1984, year_end_watch = 2007
    )
    save(df_pt_jpl, file = filn)
    df_pt_jpl %>%
      tidyr::unnest(df) %>%
      write_csv(path = filn_csv)
  } else {
    df_pt_jpl <- read_csv(file = filn_csv) %>%
      group_by(idx, lon, lat) %>%
      tidyr::nest() %>%
      dplyr::mutate(data = purrr::map(data, ~as_tibble(.))) %>%
      dplyr::rename(df = data)
  }
} else {
  load(filn)
  df_pt_jpl %>%
    tidyr::unnest(df) %>%
    write_csv(path = "data/df_pt_jpl.csv")
}


## PM
filn <- "data/df_pm_mod_sj02.Rdata"
filn_csv <- str_replace(filn, "Rdata", "csv")
if (!file.exists(filn)){
  if (!file.exists(filn_csv)){
    df_pm_mod <- get_data_mct_global(
      df_grid,
      dir_et   = "~/data/landflux/et_prod/",        fil_et_pattern   = "ET_PM-SRB-PU_daily_",
      dir_prec = "~/data/watch_wfdei/Rainf_daily/", fil_prec_pattern = "Rainf_daily_WFDEI_CRU",
      dir_snow = "~/data/watch_wfdei/Snowf_daily/", fil_snow_pattern = "Snowf_daily_WFDEI_CRU",
      dir_temp = "~/data/watch_wfdei/Tair_daily/",  fil_temp_pattern = "Tair_daily_WFDEI",
      get_watch = TRUE, get_landeval = TRUE, get_alexi = FALSE,
      year_start_watch = 1984, year_end_watch = 2007
    )
    save(df_pm_mod, file = filn)
    df_pm_mod %>%
      tidyr::unnest(df) %>%
      write_csv(path = filn_csv)
  } else {
    df_pm_mod <- read_csv(file = filn_csv) %>%
      group_by(idx, lon, lat) %>%
      tidyr::nest() %>%
      dplyr::mutate(data = purrr::map(data, ~as_tibble(.))) %>%
      dplyr::rename(df = data)
  }
} else {
  load(filn)
  df_pm_mod %>%
    tidyr::unnest(df) %>%
    write_csv(path = "data/df_pm_mod.csv")
}


## SEBS
filn <- "data/df_sebs_sj02.Rdata"
filn_csv <- str_replace(filn, "Rdata", "csv")
if (!file.exists(filn)){
  if (!file.exists(filn_csv)){
    df_sebs <- get_data_mct_global(
      df_grid,
      dir_et   = "~/data/landflux/et_prod/",        fil_et_pattern   = "ET_SEBS-SRB-PU_daily_",
      dir_prec = "~/data/watch_wfdei/Rainf_daily/", fil_prec_pattern = "Rainf_daily_WFDEI_CRU",
      dir_snow = "~/data/watch_wfdei/Snowf_daily/", fil_snow_pattern = "Snowf_daily_WFDEI_CRU",
      dir_temp = "~/data/watch_wfdei/Tair_daily/",  fil_temp_pattern = "Tair_daily_WFDEI",
      get_watch = TRUE, get_landeval = TRUE, get_alexi = FALSE,
      year_start_watch = 1984, year_end_watch = 2007
    )
    save(df_sebs, file = filn)
    df_sebs %>%
      tidyr::unnest(df) %>%
      write_csv(path = filn_csv)
  } else {
    df_sebs <- read_csv(file = filn_csv) %>% 
      group_by(idx, lon, lat) %>%
      tidyr::nest() %>% 
      dplyr::mutate(data = purrr::map(data, ~as_tibble(.)))%>% 
      dplyr::rename(df = data)
  }
} else {
  load(filn)
  df_sebs %>%
    tidyr::unnest(df) %>%
    write_csv(path = "data/df_sebs.csv")
}

##------------------------------------------------------------------------
## WATCH-WFDEI and ALEXI
##------------------------------------------------------------------------
filn <- "data/df_alexi_sj02.Rdata"
filn_csv <- str_replace(filn, "Rdata", "csv")
if (!file.exists(filn)){
  if (!file.exists(filn_csv)){
    df_alexi <- get_data_mct_global(
      df_grid,
      dir_et   = "~/data/alexi_tir/netcdf/",        fil_et_pattern   = "EDAY_CERES_",
      dir_prec = "~/data/watch_wfdei/Rainf_daily/", fil_prec_pattern = "Rainf_daily_WFDEI_CRU",
      dir_snow = "~/data/watch_wfdei/Snowf_daily/", fil_snow_pattern = "Snowf_daily_WFDEI_CRU",
      dir_temp = "~/data/watch_wfdei/Tair_daily/",  fil_temp_pattern = "Tair_daily_WFDEI",
      get_watch = TRUE, get_landeval = FALSE, get_alexi = TRUE,
      year_start_watch = 2003, year_end_watch = 2018
    )
    save(df_alexi, file = filn)
    df_alexi %>%
      tidyr::unnest(df) %>%
      write_csv(path = filn_csv)
  } else {
    df_alexi <- read_csv(file = filn_csv) %>%
      group_by(idx, lon, lat) %>%
      tidyr::nest() %>%
      dplyr::mutate(data = purrr::map(data, ~as_tibble(.))) %>%
      dplyr::rename(df = data)
  }
} else {
  load(filn)
  df_alexi %>%
    tidyr::unnest(df) %>%
    write_csv(path = "data/df_alexi.csv")
}

# ## compare
# ggplot() +
#   geom_line(data = df_pt_jpl$df[[29]], aes(x = date, y = et_mm)) +
#   geom_line(data = df_alexi$df[[70]],  aes(x = date, y = et_mm), col = 'red')
