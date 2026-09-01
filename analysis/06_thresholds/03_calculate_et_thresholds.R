#!/usr/bin/env Rscript

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(broom)
library(rlang)
library(lubridate)
library(segmented)

source("R/workflow_helpers.R")
source("R/calc_cwd_et0_byilon.R")

args <- chunk_arguments()
ilon <- parse_index_spec()
if (is.null(ilon)) ilon <- seq_len(7200L)
ilon <- work_for_chunk(ilon, args$chunk, args$chunks)

message("Calculating ET thresholds for longitude indices: ", paste(ilon, collapse = ", "))
df_out <- run_parallel(
  ilon,
  calc_cwd_et0_byilon,
  dirn = "data/df_cwd_et0_2",
  verbose = FALSE
)
