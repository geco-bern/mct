#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)
# args <- c(5, 30)

overwrite <- TRUE

library(tidyverse)
library(extRemes)

source("R/calc_return_level.R")

load("data/df_corr.RData")

df_corr <- df_corr %>% 
  dplyr::select(lon, lat, s0 = cwd_lue0_nSIF) %>% ## select which one to consider here!
  arrange(lon) %>% 
  mutate(idx = 1:n()) %>%
  mutate(chunk = rep(1:as.integer(args[2]), each = (nrow(.)/as.integer(args[2])), len = nrow(.)))

## split sites data frame into (almost) equal chunks
list_df_split <- df_corr_tmp %>%
  group_by(chunk) %>%
  group_split()

## retain only the one required for this chunk
df_corr_sub <- list_df_split[[as.integer(args[1])]]

##------------------------------------------------------------------------
## asdf
##------------------------------------------------------------------------
filn <- paste0("data/df_rl/df_rl_nSIF_ichunk_", args[1], "_", args[2], ".RData")

df_rl_diag <- df_corr_sub %>% 
  drop_na() %>% 
  group_by(lon) %>% 
  nest() %>% 
  mutate(ilon = as.integer((lon + 179.975)/0.05 + 1)) %>% 
  ungroup()

# ## xxx debug
# filn <- paste0("data/df_rl/df_rl_fet_ichunk_TEST.RData")
# df_rl_diag <- df_rl_diag %>% 
#   dplyr::filter(lon > 120 & lon < 121)

if (nrow(df_rl_diag)>0){
  if (!file.exists(filn) || overwrite){
    df <- df_rl_diag %>% 
      mutate(data = purrr::map2(ilon, data, ~calc_return_level(.x, .y))) %>% 
      unnest(data) %>% 
      dplyr::select(-ilon)
    save(df, file = filn)
  } else {
    print(paste("File exists already: ", filn))
  }
} else {
  print("No data available for this chunk.")
}
