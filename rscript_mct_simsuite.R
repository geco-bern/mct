library(dplyr)
library(rbeni)
library(tidyr)
library(purrr)
library(ncdf4)
library(lubridate)
library(extRemes)
library(R.utils)
library(readr)

source("R/mct.R")
source("R/get_plantwhc_mct_bysite.R")
source("R/get_plantwhc_mct_simsuite.R")

df_sites <- read_csv("/alphadata01/bstocker/data/rootingdepth/root_profiles_schenkjackson02/data/root_profiles_D50D95.csv") %>%
  dplyr::filter(Wetland == "N" & Anthropogenic == "N" & Schenk_Jackson_2002 == "YES") %>% 
  dplyr::rename(sitename = ID, lat = Latitude, lon = Longitude) %>% 
  dplyr::mutate(elv = ifelse(elv==-999, NA, elv)) %>% 
  dplyr::filter(lon!=-999 & lat!=-999) %>% 
  dplyr::mutate(year_start = 1982, year_end = 2011) %>% 
  dplyr::select(sitename, lon, lat, elv, year_start, year_end)

dir_climate <- "/Users/benjaminstocker/sofun/output_nc_global_sofun"
dir_fapar <- "~/sofun/input_sj02zroot_sofun/sitedata/fapar/"

df <- get_plantwhc_mct_simsuite(df_sites, dir_climate = dir_climate, dir_fapar = dir_fapar)

