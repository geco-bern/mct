#!/usr/bin/env Rscript

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(broom)
library(rlang)
library(lubridate)
library(extRemes)

source("R/workflow_helpers.R")
source("R/input_config.R")
source("R/get_cwdx_byilon.R")

args <- chunk_arguments()
config <- read_input_config()
cwdx_path <- climate_output_path("data/df_cwdx_10_20_40.RData", config)
require_files(cwdx_path, "failed-cell repair")
load(cwdx_path)

df_nested <- df %>%
  dplyr::filter(is.na(cwdx20), lat > -60, lat < 75) %>%
  mutate(
    lon = round(lon, digits = 3),
    lat = round(lat, digits = 3),
    ilon = as.integer(round(
      (lon - config$et$source$grid$longitude_start) /
        config$et$source$grid$longitude_step + 1
    ))
  ) %>%
  dplyr::select(lat, ilon) %>%
  group_by(ilon) %>%
  nest() %>%
  mutate(ncells = purrr::map_int(data, nrow)) %>%
  ungroup()

ilon <- parse_index_spec()
if (is.null(ilon)) ilon <- df_nested$ilon
ilon <- work_for_chunk(ilon, args$chunk, args$chunks)
df_nested <- df_nested %>% dplyr::filter(ilon %in% !!ilon)

repair_band <- function(index) {
  get_cwdx_byilon(
    df_nested$ilon[[index]],
    df_lat = df_nested$data[[index]],
    config = config
  )
}

message("Repairing longitude indices: ", paste(df_nested$ilon, collapse = ", "))
df_out <- run_parallel(seq_len(nrow(df_nested)), repair_band)
