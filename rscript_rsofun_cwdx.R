#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)

# set output path
path <- "./data/out_rsofun_cwdx/"

library(tidyverse)
library(rsofun)
library(ingestr)
library(rbeni)
library(segmented)
library(extRemes)

source("R/process_pmodel.R")
source("R/mct2.R")
source("R/get_plantwhc_mct_bysite.R")
source("R/calc_cwd_lue0_v2.R")
source("R/extract_return_level.R")
source("R/run_rsofun_cwdx_by_chunk.R")

use_whc <- ifelse(args[2] == "9999", NA, as.numeric(args[2]))

out <- run_rsofun_cwdx_by_chunk(as.character(args[1]), use_whc = use_whc)
filename <- file.path(path, paste0("out_rsofun_cwdx_whc_", as.character(use_whc), "_ichunk_", as.character(args[1]), ".rds"))
print(paste("saving", filename, "..."))
saveRDS(out, filename)
