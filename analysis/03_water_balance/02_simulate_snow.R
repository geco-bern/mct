#!/usr/bin/env Rscript

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)

source("R/workflow_helpers.R")
source("R/input_config.R")
source("R/simulate_snow_byilon.R")

args <- chunk_arguments()
config <- read_input_config()
ilon <- parse_index_spec()
if (is.null(ilon)) ilon <- seq_len(config$precipitation$rain$grid$longitude_count)
ilon <- work_for_chunk(ilon, args$chunk, args$chunks)

message("Simulating snow for longitude indices: ", paste(ilon, collapse = ", "))
df_out <- run_parallel(ilon, simulate_snow_byilon, config = config)
