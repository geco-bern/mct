#!/usr/bin/env Rscript

library(dplyr)

source("R/input_config.R")

config <- read_input_config()

tagged_exists <- function(path) {
  file.exists(path.expand(climate_output_path(path, config)))
}

check_avail_et_tidy <- function(ilon) {
  file.exists(source_tidy_path(config$et$source, ilon, config))
}

check_avail_et_mm <- function(ilon) {
  tagged_exists(paste0("data/df_et_mm/df_et_mm_ilon_", ilon, ".RData"))
}

check_avail_snow <- function(ilon) {
  tagged_exists(paste0("data/df_snow/df_snow_ilon_", ilon, ".RData"))
}

check_avail_bal <- function(ilon) {
  tagged_exists(paste0("data/df_bal/df_bal_ilon_", ilon, ".RData"))
}

check_avail_cwdx <- function(ilon) {
  tagged_exists(paste0("data/df_cwdx/df_cwdx_ilon_", ilon, ".RData"))
}

check_avail_10_20_40 <- function(ilon) {
  tagged_exists(paste0(
    "data/df_cwdx_10_20_40/df_cwdx_10_20_40_ilon_",
    ilon,
    ".RData"
  ))
}

check_avail_cwd_et0 <- function(ilon) {
  tagged_exists(paste0("data/df_cwd_et0_2/df_cwd_et0_", ilon, ".RData"))
}

check_avail_cwd_lue0 <- function(ilon) {
  tagged_exists(paste0("data/df_cwd_lue0_2/df_cwd_lue0_", ilon, ".RData"))
}

longitude_count <- config$et$source$grid$longitude_count
df <- tibble(ilon = seq_len(longitude_count)) %>%
  mutate(
    ilon_lores = nearest_source_indices(
      ilon,
      from_source = config$et$source,
      to_source = config$precipitation$rain
    ),
    avl_tidy = purrr::map_lgl(ilon, check_avail_et_tidy),
    avl_et_mm = purrr::map_lgl(ilon, check_avail_et_mm),
    avl_snow = purrr::map_lgl(ilon_lores, check_avail_snow),
    avl_bal = purrr::map_lgl(ilon, check_avail_bal),
    avl_cwdx = purrr::map_lgl(ilon, check_avail_cwdx),
    avl_cwdx_10_20_40 = purrr::map_lgl(ilon, check_avail_10_20_40),
    avl_cwd_et0 = purrr::map_lgl(ilon, check_avail_cwd_et0),
    avl_cwd_lue0 = purrr::map_lgl(ilon, check_avail_cwd_lue0)
  )

save(
  df,
  file = climate_output_path("data/df_file_availability.RData", config)
)
