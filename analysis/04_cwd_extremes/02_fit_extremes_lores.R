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
source("R/get_cwdx_byilon_lores.R")

args <- chunk_arguments()
config <- read_input_config()
ilon <- parse_index_spec()
if (is.null(ilon)) ilon <- seq_len(config$et$low_resolution_source$grid$longitude_count)
ilon <- work_for_chunk(ilon, args$chunk, args$chunks)

message("Fitting low-resolution extremes for longitude indices: ", paste(ilon, collapse = ", "))
df_out <- run_parallel(ilon, get_cwdx_byilon_lores, config = config)
