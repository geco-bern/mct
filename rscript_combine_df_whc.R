#!/usr/bin/env Rscript

library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(multidplyr)
library(rlang)

source("R/extract_whc_byfil.R")

dir <- "data/df_whc_hires_chunks/"
filelist <- paste0(dir, list.files(dir, pattern = "df_whc_hires_chunk_.*.RData"))

## get all available cores
ncores <- parallel::detectCores()

if (ncores > 1){
  
  cl <- multidplyr::new_cluster(ncores) %>%
    multidplyr::cluster_library(c("dplyr", "tidyr", "magrittr")) %>%
    multidplyr::cluster_assign(extract_whc_byfil = extract_whc_byfil)
  
  ## distribute to cores, making sure all data from a specific site is sent to the same core
  df_whc <- tibble(ifil = filelist) %>%
    multidplyr::partition(cl) %>%
    dplyr::mutate(out = purrr::map( ifil,
                                    ~extract_whc_byfil(.))) %>% 
    collect() %>% 
    bind_rows()
  
} else {
  
  ## testing
  df_whc <- purrr::map(as.list(filelist), 
                       ~extract_whc_byfil(.)) %>% 
    bind_rows()
  
}

save(df_whc, file = "~/data/mct_data/df_whc_hires_lasthope.RData")


## test plot
gg <- df_whc %>% 
  mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) %>% 
  plot_map3(varnam = "whc_top", lonmin = -80, lonmax = -60, latmin = -60, latmax = -35, 
            breaks = c(seq(0, 0.4, by = 0.05), 0.8, Inf), 
            spacing = "constant",
            combine = FALSE, 
            colorscale = viridis::magma
  )
gg$ggmap <- gg$ggmap + labs(title = "WHC", subtitle = "Topsoil, fraction")
cowplot::plot_grid(gg$ggmap, gg$gglegend, ncol = 2, rel_widths = c(1, 0.2))
>>>>>>> 0d8a9841fb687049b6d1577f3ab623f8c715f51c
