#!/usr/bin/env Rscript

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)

source("R/workflow_helpers.R")
source("R/get_bal_byilon.R")

args <- chunk_arguments()
ilon <- parse_index_spec()
if (is.null(ilon)) ilon <- seq_len(7200L)
ilon <- work_for_chunk(ilon, args$chunk, args$chunks)

message("Calculating balance for longitude indices: ", paste(ilon, collapse = ", "))
df_out <- run_parallel(ilon, get_bal_byilon)
