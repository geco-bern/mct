library(dplyr)
library(purrr)
library(rbeni)
library(stringr)

devtools::install_github("stineb/rbeni")

dirn <- "/alphadata01/bstocker/data/modis_monthly-evi/zmaw_data/0_05deg/"
files <- list.files(path = dirn, pattern = "modis_vegetation__LPDAAC__v5__0.05deg", recursive = TRUE )
files <- tibble(filnam = files) %>% 
  dplyr::filter(!str_detect(filnam, "halfdeg")) %>% 
  dplyr::filter(!str_detect(filnam, "txt")) %>% 
  #dplyr::filter(str_detect(filnam, "201")) %>% 
  dplyr::select(filnam) %>% 
  unlist() %>% 
  unname() %>% 
  paste0(dirn, .)

purrr::map(
  as.list(files), 
  ~regrid_nc(obj = ., varname = "evi", method = "max", outgrid = "halfdeg", returnobj = FALSE))
