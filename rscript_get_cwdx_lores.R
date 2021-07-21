#!/usr/bin/env Rscript

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(broom)
library(rlang)
library(lubridate)
library(extRemes)

source("R/get_cwdx_byilon_lores.R")

df_out <- purrr::map(as.list(seq(720)), ~get_cwdx_byilon_lores(.))
