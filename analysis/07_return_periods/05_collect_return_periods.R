library(tidyverse)
source("R/collect_rp_diag.R")

df_rp_diag_nSIF <- purrr::map_dfr(as.list(seq(30)),
           ~collect_rp_diag_nSIF(.))

df_rp_diag_fet <- purrr::map_dfr(as.list(seq(30)),
                                  ~collect_rp_diag_fet(.))

save(df_rp_diag_nSIF, file = "data/df_rp_diag_nSIF.RData")
save(df_rp_diag_fet, file = "data/df_rp_diag_fet.RData")