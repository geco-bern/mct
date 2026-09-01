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
source("R/get_cwdx_byilon.R")

args <- chunk_arguments()
require_files("data/df_cwdx_10_20_40.RData", "failed-cell repair")
load("data/df_cwdx_10_20_40.RData")

df_nested <- df %>%
  dplyr::filter(is.na(cwdx20), lat > -60, lat < 75) %>%
  mutate(
    lon = round(lon, digits = 3),
    lat = round(lat, digits = 3),
    ilon = as.integer(round((lon + 179.975) / 0.05 + 1))
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
  get_cwdx_byilon(df_nested$ilon[[index]], df_lat = df_nested$data[[index]])
}

message("Repairing longitude indices: ", paste(df_nested$ilon, collapse = ", "))
df_out <- run_parallel(seq_len(nrow(df_nested)), repair_band)
