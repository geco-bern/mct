#!/usr/bin/env Rscript

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(rlang)

source("R/workflow_helpers.R")
source("R/collect_cwd_lue0_byilon.R")

df <- run_parallel(
  seq_len(7200L),
  collect_cwd_lue0_byilon,
  continue_on_error = FALSE
) %>% bind_rows()

path <- "data/df_cwd_lue0_2.RData"
write_rdata_atomic(df, "df", path.expand(path))

vec_lon_avl <- round(unique(df$lon), digits = 3)
vec_lon_hires <- round(seq(-179.975, 179.975, by = 0.05), digits = 3)
vec_lon_missing <- vec_lon_hires[!(vec_lon_hires %in% vec_lon_avl)]
vec_ilon_missing <- (vec_lon_missing + 179.975) / 0.05 + 1
write_rdata_atomic(
  vec_ilon_missing,
  "vec_ilon_missing",
  "data/vec_ilon_missing.RData"
)
