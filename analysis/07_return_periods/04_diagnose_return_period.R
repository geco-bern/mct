#!/usr/bin/env Rscript

library(tidyverse)

source("R/workflow_helpers.R")
source("R/calc_return_period.R")

args <- chunk_arguments()
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

  filn <- paste0(
    "data/df_rp_diag/df_rp_diag_", label,
    "_ichunk_", args$chunk, "_", args$chunks, ".RData"
  )

  df_rp_diag <- df_corr %>%
    dplyr::select(lon, lat, s0 = !!signal) %>%
    drop_na() %>%
    group_by(lon) %>%
    nest() %>%
    mutate(ilon = as.integer(round((lon + 179.975) / 0.05 + 1))) %>%
    ungroup()

  if (!nrow(df_rp_diag)) {
    message("No ", label, " data available for this chunk.")
  } else if (file.exists(filn)) {
    message("File exists already: ", filn)
  } else {
    df_rp_diag <- df_rp_diag %>%
      mutate(data = purrr::map2(ilon, data, calc_return_period)) %>%
      unnest(data) %>%
      dplyr::select(lon, lat, loc, scale, rp_diag)
    write_rdata_atomic(df_rp_diag, "df_rp_diag", filn)
    message("Wrote ", filn)
  }
}

if (file.exists("data/df_corr_nSIF.RData")) {
  load("data/df_corr_nSIF.RData")
  diagnose_return_period(df_corr_nSIF, cwd_lue0_nSIF, "nSIF")
} else {
  warning("Missing data/df_corr_nSIF.RData; skipping nSIF.", call. = FALSE)
}

if (file.exists("data/df_corr_fet.RData")) {
  load("data/df_corr_fet.RData")
  diagnose_return_period(df_corr_fet, cwd_lue0_fet, "fet")
} else {
  warning("Missing data/df_corr_fet.RData; skipping fet.", call. = FALSE)
}
