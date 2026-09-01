#!/usr/bin/env Rscript

source(here::here("R", "workflow_helpers.R"))
source(here::here("R", "input_config.R"))

config <- read_input_config()

jobs <- read.delim(
  project_path("src", "ubelix", "jobs.tsv"),
  sep = "\t",
  quote = "",
  comment.char = "",
  stringsAsFactors = FALSE
)

source_output_pattern <- function(source) {
  climate_output_path(
    file.path(
      path.expand(source$tidy_dir),
      paste0(source$tidy_prefix, "_ilon_*.RData")
    ),
    config
  )
}

source_patterns <- c(
  "@precipitation_tidy" = source_output_pattern(config$precipitation$rain),
  "@snow_tidy" = source_output_pattern(config$precipitation$snow),
  "@temperature_tidy" = source_output_pattern(config$temperature$source),
  "@et_tidy" = source_output_pattern(config$et$source),
  "@et_lores_tidy" = source_output_pattern(config$et$low_resolution_source)
)

run_specific_stages <- c(
  "01_tidy_inputs", "03_water_balance", "04_cwd_extremes",
  "06_thresholds", "07_return_periods", "09_diagnostics"
)

resolve_output_pattern <- function(pattern, stage) {
  if (!nzchar(pattern) || pattern == "-") return(pattern)
  if (pattern %in% names(source_patterns)) return(unname(source_patterns[[pattern]]))
  if (!stage %in% run_specific_stages) return(pattern)
  paste(
    vapply(
      strsplit(pattern, ";", fixed = TRUE)[[1]],
      climate_output_path,
      character(1),
      config = config
    ),
    collapse = ";"
  )
}

jobs$output_pattern <- mapply(
  resolve_output_pattern,
  jobs$output_pattern,
  jobs$stage,
  USE.NAMES = FALSE
)

hires_count <- config$et$source$grid$longitude_count
lores_count <- config$et$low_resolution_source$grid$longitude_count
forcing_count <- config$precipitation$rain$grid$longitude_count
jobs$expected_outputs[jobs$job %in% c(
  "prepare_et", "sif_jj", "sif_pk", "glass", "convert_et_mm",
  "calculate_balance", "fit_extremes", "extract_return_levels",
  "redo_failed", "calculate_sif_thresholds", "calculate_et_thresholds"
)] <- hires_count
jobs$expected_outputs[jobs$job %in% c(
  "prepare_precipitation", "prepare_snowfall", "prepare_temperature",
  "watch_swrad", "simulate_snow"
)] <- forcing_count
jobs$expected_outputs[jobs$job %in% c(
  "prepare_et_lores", "sif_jj_lores", "sif_pk_lores",
  "calculate_balance_lores", "fit_extremes_lores"
)] <- lores_count

count_outputs <- function(pattern) {
  if (!nzchar(pattern) || pattern == "-") return(NA_integer_)
  patterns <- strsplit(pattern, ";", fixed = TRUE)[[1]]
  sum(vapply(patterns, function(item) {
    length(Sys.glob(path.expand(item)))
  }, integer(1)))
}

jobs$outputs_found <- vapply(jobs$output_pattern, count_outputs, integer(1))
jobs$status <- ifelse(
  is.na(jobs$expected_outputs),
  ifelse(jobs$outputs_found > 0L, "started", "not started"),
  ifelse(jobs$outputs_found >= jobs$expected_outputs, "complete", "incomplete")
)

print(
  jobs[c("stage", "job", "outputs_found", "expected_outputs", "status")],
  row.names = FALSE
)
