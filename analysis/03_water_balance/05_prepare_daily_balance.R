#!/usr/bin/env Rscript

# Prepare daily water-balance inputs
# Extracted from vignettes/archive/workflow_legacy.Rmd.
source("analysis/_common.R")

n_avl_et <- function(df){
  sum(!is.na(df$et))
}

df_alexi <- df_alexi |> 
  mutate(avl_et = purrr::map_int(df, ~n_avl_et(.))) 

## where are they? They are along coasts. Ok, valid to remove them.
plot_map_simpl() +
  geom_point(
    data = df_alexi |> 
      dplyr::filter(avl_et < 3000),
    aes(lon, lat),
    color = 'red')

df_alexi <- df_alexi |> 
  dplyr::filter(avl_et > 3000)

fileprefix <- "global_FULL_MODIS-C006_MOD15A2_v3.4.d.pet"
dir <- "~/data/sofun_outputs/global_FULL_MODIS-C006_MOD15A2_v3.4/"
nclist <- paste0(dir, list.files(dir, pattern = paste0(fileprefix, ".*.nc"), recursive = TRUE))

## create files for each longitude slice, containing full time series wrapped for each gridcell (latitude)
nclist_to_df(
  nclist, 
  outdir = "~/data/sofun_outputs/global_FULL_MODIS-C006_MOD15A2_v3.4/data_tidy/", 
  fileprefix = fileprefix, 
  varnam = "pet", 
  lonnam = "lon", 
  timenam = "time", 
  timedimnam = "time", 
  ncores = 1, 
  single_basedate = TRUE
  )

source("R/get_bal_byilon_lores.R")
df_out <- purrr::map(as.list(seq(720)), ~get_bal_byilon_lores(.))

source("R/get_cwdx_byilon_lores.R")
df_out <- purrr::map(as.list(seq(720)), ~get_cwdx_byilon_lores(.))

load("data/df_file_availability.RData")

df |> 
  dplyr::filter(!avl_snow)

df |> 
  dplyr::filter(!avl_tidy)

df |> 
  dplyr::filter(!avl_et_mm)

df |> 
  dplyr::filter(!avl_bal)

df |> 
  ungroup() |> 
  dplyr::filter(!avl_cwdx)

df |> 
  dplyr::filter(!avl_cwdx_10_20_40 & avl_cwdx & avl_bal & avl_et_mm)

df |> 
  ungroup() |> 
  dplyr::filter(!avl_cwdx_10_20_40)

