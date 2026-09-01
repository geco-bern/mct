#!/usr/bin/env Rscript

library(tidyverse)
library(extRemes)

source("R/workflow_helpers.R")
source("R/calc_return_level.R")

chunk_info <- chunk_arguments()
args <- c(chunk_info$chunk, chunk_info$chunks)
overwrite <- identical(tolower(Sys.getenv("MCT_OVERWRITE", "false")), "true")
ensure_directory("data/df_rl")

load("data/df_corr.RData")

df_corr_tmp <- df_corr %>% 
  dplyr::select(lon, lat, s0 = cwd_lue0_nSIF) %>% ## select which one to consider here!
  arrange(lon) %>% 
  mutate(idx = 1:n()) %>%
  mutate(chunk = ceiling(idx / ceiling(nrow(.) / as.integer(args[2]))))

df_corr_sub <- df_corr_tmp %>%
  dplyr::filter(chunk == as.integer(args[1]))

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
