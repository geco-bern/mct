#!/usr/bin/env Rscript

source("R/workflow_helpers.R")
source("R/input_config.R")
source("R/map_netcdf_to_tidy.R")

args <- chunk_arguments()
config <- read_input_config()
source_config <- config$et$source
nclist <- source_netcdf_files(source_config)

map_netcdf_to_tidy(
  nclist,
  source_config$tidy_dir,
  source_config$tidy_prefix,
  source_config$variable,
  source_config$longitude_name,
  source_config$latitude_name,
  source_config$time_name,
  chunk = args$chunk,
  chunks = args$chunks,
  ilon = parse_index_spec(),
  output_tag = climate_run_id(config)
)
