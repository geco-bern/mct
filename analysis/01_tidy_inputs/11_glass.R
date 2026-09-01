#!/usr/bin/env Rscript

source("R/workflow_helpers.R")
source("R/map_netcdf_to_tidy.R")

args <- chunk_arguments()
dir <- "~/data/glass/data_netcdf/"
fileprefix <- "GLASS07B01.V41."
nclist <- list.files(
  path.expand(dir),
  pattern = "^GLASS07B01[.]V41[.].*[.]nc$",
  recursive = TRUE,
  full.names = TRUE
)
outdir <- "~/data/glass/data_tidy/"
varnam <- "NR"
lonnam <- "lon"
latnam <- "lat"
timenam <- "time"

map_netcdf_to_tidy(
  nclist, outdir, fileprefix, varnam, lonnam, latnam, timenam,
  chunk = args$chunk, chunks = args$chunks, ilon = parse_index_spec()
)
