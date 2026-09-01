#!/usr/bin/env Rscript

source("R/workflow_helpers.R")
source("R/map_netcdf_to_tidy.R")

args <- chunk_arguments()
dir <- "~/data/gome_2_sif_downscaled/data_halfdeg/"
fileprefix <- "GOME_JJ_dcSIF_05deg_8day_"
nclist <- list.files(
  path.expand(dir),
  pattern = paste0(fileprefix, ".*[.]nc$"),
  full.names = TRUE
)
outdir <- "~/data/gome_2_sif_downscaled/data_tidy_halfdeg/"
varnam <- "SIF"
lonnam <- "lon"
latnam <- "lat"
timenam <- "time"

map_netcdf_to_tidy(
  nclist, outdir, fileprefix, varnam, lonnam, latnam, timenam,
  chunk = args$chunk, chunks = args$chunks, ilon = parse_index_spec()
)
