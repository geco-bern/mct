#!/usr/bin/env Rscript

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(rlang)
library(lubridate)

source("R/workflow_helpers.R")
source("R/input_config.R")
source("R/extract_cwdx_byilon.R")

args <- chunk_arguments()
config <- read_input_config()
ilon <- parse_index_spec()
if (is.null(ilon)) ilon <- seq_len(config$et$source$grid$longitude_count)
ilon <- work_for_chunk(ilon, args$chunk, args$chunks)

message("Extracting return levels for longitude indices: ", paste(ilon, collapse = ", "))
df <- run_parallel(ilon, extract_cwdx_byilon, overwrite = FALSE, config = config)
