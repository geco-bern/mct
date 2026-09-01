#!/usr/bin/env Rscript

library(tidyverse)

source("R/workflow_helpers.R")
source("R/input_config.R")
source("R/calc_return_period.R")

args <- chunk_arguments()
config <- read_input_config()
ensure_directory("data/df_rp_diag")

diagnose_return_period <- function(df_corr, signal, label) {
  signal <- rlang::ensym(signal)
  df_corr <- df_corr %>%
    arrange(lon) %>%
    mutate(
      idx = seq_len(n()),
      chunk = ceiling(idx / ceiling(nrow(.) / args$chunks))
    ) %>%
    dplyr::filter(chunk == args$chunk)

  filn <- climate_output_path(paste0(
    "data/df_rp_diag/df_rp_diag_", label,
    "_ichunk_", args$chunk, "_", args$chunks, ".RData"
  ), config)

  df_rp_diag <- df_corr %>%
    dplyr::select(lon, lat, s0 = !!signal) %>%
    drop_na() %>%
    group_by(lon) %>%
    nest() %>%
    mutate(ilon = source_longitude_index(lon, config$et$source)) %>%
    ungroup()

  if (!nrow(df_rp_diag)) {
    message("No ", label, " data available for this chunk.")
  } else if (file.exists(filn)) {
    message("File exists already: ", filn)
  } else {
    df_rp_diag <- df_rp_diag %>%
      mutate(data = purrr::map2(ilon, data, ~calc_return_period(.x, .y, config))) %>%
      unnest(data) %>%
      dplyr::select(lon, lat, loc, scale, rp_diag)
    write_rdata_atomic(df_rp_diag, "df_rp_diag", filn)
    message("Wrote ", filn)
  }
}

path_corr <- climate_output_path("data/df_corr.RData", config)
if (file.exists(path_corr)) {
  load(path_corr)
  diagnose_return_period(df_corr, cwd_lue0_nSIF, "nSIF")
  diagnose_return_period(df_corr, cwd_lue0_fet, "fet")
} else {
  warning("Missing ", path_corr, "; skipping return-period diagnosis.", call. = FALSE)
}
