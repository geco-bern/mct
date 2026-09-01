#!/usr/bin/env Rscript

source("R/workflow_helpers.R")
source("R/input_config.R")
source("R/map_netcdf_to_tidy.R")

args <- chunk_arguments()
config <- read_input_config()
fileprefix <- "SWdown_daily_WFDEI_"
dir <- "~/data/watch_wfdei/"
nclist <- list.files(
  path.expand(dir),
  pattern = paste0(fileprefix, ".*[.]nc$"),
  recursive = TRUE,
  full.names = TRUE
)
outdir <- "~/data/watch_wfdei/data_tidy/"
varnam <- "SWdown"
lonnam <- "lon"
latnam <- "lat"
timenam <- "timestp"

map_netcdf_to_tidy(
  nclist, outdir, fileprefix, varnam, lonnam, latnam, timenam,
  chunk = args$chunk, chunks = args$chunks, ilon = parse_index_spec(),
  output_tag = climate_run_id(config)
)
