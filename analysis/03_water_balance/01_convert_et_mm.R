#!/usr/bin/env Rscript

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)

source("R/workflow_helpers.R")
source("R/get_et_mm_bylon.R")

args <- chunk_arguments()
ilon <- parse_index_spec()
if (is.null(ilon)) ilon <- seq_len(7200L)
ilon <- work_for_chunk(ilon, args$chunk, args$chunks)

message("Converting ET for longitude indices: ", paste(ilon, collapse = ", "))
df_out <- run_parallel(ilon, get_et_mm_byilon)
