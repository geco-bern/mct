#!/usr/bin/env Rscript

# args = commandArgs(trailingOnly=TRUE)
args <- c(10, 30)

library(tidyverse)

# source("R/calc_return_period.R")

load("data/df_corr.RData")

df_corr <- df_corr %>% 
  mutate(idx = 1:n()) %>%
  mutate(chunk = rep(1:as.integer(args[2]), each = (nrow(.)/as.integer(args[2])), len = nrow(.)))

## split sites data frame into (almost) equal chunks
list_df_split <- df_corr %>%
  group_by(chunk) %>%
  group_split()

## retain only the one required for this chunk
df_corr_sub <- list_df_split[[as.integer(args[1])]]

print("This chunk contains these rows of the full site data frame:")
print(df_corr_sub$idx)

##------------------------------------------------------------------------
## ingest forcing data, run P-model, and get climate indeces at once
##------------------------------------------------------------------------
filn <- paste0("data/df_rp_diag/df_rp_diag_ichunk_", args[1], "_", args[2], ".RData")
if (!file.exists(filn)){
	df_rp_diag <- df_corr_sub %>% 
	  dplyr::select(lon, lat, cwd_lue0_nSIF) %>% 
	  drop_na() %>% 
	  group_by(lon) %>% 
	  nest() %>% 
	  mutate(ilon = as.integer((lon + 179.975)/0.05 + 1)) %>% 
	  ungroup() %>% 
	  mutate(data = purrr::map2(ilon, data, ~calc_return_period(.x, .y))) %>% 
	  unnest(data) %>% 
	  dplyr::select(lon, lat, loc, scale, rp_diag)
  save(df_rp_diag, file = filn)
} else {
  print(paste("File exists already: ", filn))
}
