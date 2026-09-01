library(tidyverse)
source("R/input_config.R")
source("R/collect_rl.R")
config <- read_input_config()

df_rl_nSIF <- purrr::map_dfr(as.list(seq(30)),
           ~collect_rl_nSIF(., config))

df_rl_fet <- purrr::map_dfr(as.list(seq(30)),
                                  ~collect_rl_fet(., config))

save(df_rl_nSIF, file = climate_output_path("data/df_rl_nSIF.RData", config))
save(df_rl_fet, file = climate_output_path("data/df_rl_fet.RData", config))
