#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)

## xxx debug
# args <- c(1, 50, "FALSE")

# set output path
path <- "./data/forcing_rsip/"

# load libraries and
# scripts
library(tidyverse)
library(ingestr)
library(rsofun)
library(rbeni)
library(raster)
library(sf)

source("R/format_drivers.R")
source("R/process_pmodel.R")

# read sites data frame
df <- read_csv("data/df_sites_rsip.csv")

# chunk data for processing
df_sites <- df %>%
  dplyr::select(sitename, lat, lon, year_start, year_end, elv) %>%
  mutate(idx = 1:n()) %>%
  mutate(chunk = rep(1:as.integer(args[2]),
                     each = (nrow(.)/as.integer(args[2])), len = nrow(.)))

# split sites data frame into (almost) equal chunks
list_df_split <- df_sites %>%
  group_by(chunk) %>%
  group_split()

df_sites_sub <- list_df_split[[as.integer(args[1])]] %>%
  dplyr::select(-c(chunk, idx))

message("Processing pixels:")
message(nrow(df_sites_sub))

# process data
df_pmodel <- format_drivers(
    df_sites_sub,
    bias_correction = TRUE,
    verbose = TRUE,
    run_model = as.logical(args[3])
    )

# neither rowwise or apply()
# retain the tibble class which
# fucks up model evaluation in rsofun
# so a simple for loop it is

if(args[3]){
  filename <- file.path(path, paste0("pmodel_output_",args[1],".rds"))
  saveRDS(df_pmodel, filename)
} else {
  filename <- file.path(path, paste0("pmodel_drivers_",args[1],".rds"))
  saveRDS(df_pmodel, filename)
}
