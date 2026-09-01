library(ingestr)
library(segmented)
source("R/input_config.R")
source("R/calc_cwd_et0_byilon.R")

config <- read_input_config()
lon_hires <- source_grid_values(config$et$source, "longitude")
ilon <- purrr::map_int(as.list(df$lon), ~which.min(abs(. - lon_hires)))

dfs <- siteinfo_fluxnet2015 %>% 
  dplyr::select(sitename, lon, lat) %>% 
  
  ## get hires longitude index
  mutate(ilon = ilon) %>% 
  group_by(ilon) %>% 
  nest() %>% 
  rename(siteinfo = data) %>% 
  
  ungroup() %>% 
  slice(1) %>% 
  
  mutate(data = purrr::map2(ilon, siteinfo, ~calc_cwd_et0_byilon(.x, siteinfo = .y, drop_data = FALSE, overwrite = TRUE, do_plot = TRUE, config = config)))

saveRDS(
  dfs,
  file = climate_output_path("data/sample_checks_fluxnet2015sites.rds", config)
)

## One plot can then be shown as
# dfs$data[[1]]$out_lue0_fet[[1]]$gg + labs(subtitle = dfs$siteinfo[[1]]$sitename)
