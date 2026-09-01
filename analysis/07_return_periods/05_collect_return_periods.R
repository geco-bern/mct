library(tidyverse)
source("R/input_config.R")
source("R/collect_rp_diag.R")
config <- read_input_config()

df_rp_diag_nSIF <- purrr::map_dfr(as.list(seq(30)),
           ~collect_rp_diag_nSIF(., config))

df_rp_diag_fet <- purrr::map_dfr(as.list(seq(30)),
                                  ~collect_rp_diag_fet(., config))

save(df_rp_diag_nSIF, file = climate_output_path("data/df_rp_diag_nSIF.RData", config))
save(df_rp_diag_fet, file = climate_output_path("data/df_rp_diag_fet.RData", config))
