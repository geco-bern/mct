#!/usr/bin/env Rscript

source("R/workflow_helpers.R")
source("R/map_netcdf_to_tidy.R")

args <- chunk_arguments()
dir <- "~/data/alexi_tir/data_halfdeg/"
nclist <- list.files(
  path.expand(dir),
  pattern = "^EDAY_CERES_.*[.]nc$",
  full.names = TRUE
)
outdir <- "~/data/alexi_tir/data_tidy_halfdeg/"
varnam <- "et"
lonnam <- "lon"
latnam <- "lat"
timenam <- "time"
fileprefix <- "EDAY_CERES_"

map_netcdf_to_tidy(
  nclist, outdir, fileprefix, varnam, lonnam, latnam, timenam,
  chunk = args$chunk, chunks = args$chunks, ilon = parse_index_spec()
)
