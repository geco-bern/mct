#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)
# args <- c(5, 30)

library(tidyverse)
library(extRemes)

source("R/calc_return_level.R")

load("data/df_corr_fet.RData")  # must use df_corr_fet, and cwd_lue0_fet below
# load("data/df_corr_nSIF.RData") # must use df_corr_nSIF, and cwd_lue0_nSIF below

df_corr <- df_corr_fet %>% 
  arrange(lon) %>% 
  mutate(idx = 1:n()) %>%
  mutate(chunk = rep(1:as.integer(args[2]), each = (nrow(.)/as.integer(args[2])), len = nrow(.)))

## split sites data frame into (almost) equal chunks
list_df_split <- df_corr %>%
  group_by(chunk) %>%
  group_split()

## retain only the one required for this chunk
df_corr_sub <- list_df_split[[as.integer(args[1])]]

##------------------------------------------------------------------------
## asdf
##------------------------------------------------------------------------
filn <- paste0("data/df_rl/df_rl_fet_ichunk_", args[1], "_", args[2], ".RData")
df_rl_diag <- df_corr_sub %>% 
  dplyr::select(lon, lat, s0 = cwd_lue0_fet) %>% ## select which one to consider here!
  drop_na() %>% 
  group_by(lon) %>% 
  nest() %>% 
  mutate(ilon = as.integer((lon + 179.975)/0.05 + 1)) %>% 
  ungroup()

if (nrow(df_rl_diag)>0){
  if (!file.exists(filn)){
    df_rl_diag <- df_rl_diag %>% 
      mutate(data = purrr::map2(ilon, data, ~calc_return_level(.x, .y)))
    save(df_rl_diag, file = filn)
  } else {
    print(paste("File exists already: ", filn))
  }
} else {
  print("No data available for this chunk.")
}
