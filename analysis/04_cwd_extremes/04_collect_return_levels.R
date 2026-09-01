#!/usr/bin/env Rscript

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(rlang)

source("R/workflow_helpers.R")
source("R/input_config.R")
source("R/collect_cwdx_byilon.R")

config <- read_input_config()
ilon <- seq_len(config$et$source$grid$longitude_count)
message("Collecting ", length(ilon), " longitude bands.")
df <- run_parallel(
  ilon,
  collect_cwdx_byilon,
  config = config,
  continue_on_error = FALSE
) %>% bind_rows()

path <- climate_output_path("data/df_cwdx_10_20_40.RData", config)
write_rdata_atomic(df, "df", path.expand(path))
message("Wrote ", path)
