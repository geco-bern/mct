library(dplyr)
library(purrr)

files <- list.files(path = "~/data/modis_monthly-evi/zmaw_data/0_05deg", pattern = "modis_vegetation__LPDAAC__v5__0.05deg", recursive = TRUE )

files <- tibble(filnam = files) %>% 
  dplyr::filter(!str_detect(filnam, "halfdeg")) %>% 
  dplyr::select(filnam) %>% 
  unlist() %>% 
  unname()

purrr::map(
  as.list(files), 
  regrid_nc(obj = ., varname = "evi", method = "max", outgrid = "halfdeg", returnobj = FALSE))
