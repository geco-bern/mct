#!/usr/bin/env Rscript

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(rlang)

source("R/workflow_helpers.R")
source("R/input_config.R")
source("R/collect_cwd_et0_byilon.R")

config <- read_input_config()
df <- run_parallel(
  seq_len(config$et$source$grid$longitude_count),
  collect_cwd_et0_byilon,
  config = config,
  continue_on_error = FALSE
) %>% bind_rows()

path <- climate_output_path("data/df_cwd_et0_3.RData", config)
write_rdata_atomic(df, "df", path.expand(path))

vec_lon_avl <- round(unique(df$lon), digits = 3)
vec_lon_hires <- round(source_grid_values(config$et$source, "longitude"), digits = 3)
vec_lon_missing <- vec_lon_hires[!(vec_lon_hires %in% vec_lon_avl)]
vec_ilon_missing <- which(vec_lon_hires %in% vec_lon_missing)
write_rdata_atomic(
  vec_ilon_missing,
  "vec_ilon_missing",
  climate_output_path("data/vec_ilon_missing_et.RData", config)
)
