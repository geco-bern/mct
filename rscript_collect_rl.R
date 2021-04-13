library(tidyverse)
source("R/collect_rl.R")

# df_rl_nSIF <- purrr::map_dfr(as.list(seq(30)),
#            ~collect_rl_nSIF(.))

df_rl_fet <- purrr::map_dfr(as.list(seq(30)),
                                  ~collect_rl_fet(.))

# save(df_rl_nSIF, file = "data/df_rl_nSIF.RData")
save(df_rl_fet, file = "data/df_rl_fet.RData")
