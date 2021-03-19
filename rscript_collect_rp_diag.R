library(tidyverse)
source("R/collect_rp_diag.R")
df_rp_diag <- purrr::map_dfr(as.list(seq(30)),
           ~collect_rp_diag(.))

save(df_rp_diag, file = "data/df_rp_diag.RData")

load("data/df_corr.RData")

df_rp_diag %>% 
  ggplot(aes(x = 1, y = log(rp_diag))) + 
  geom_violin() +
  ylim(0, 200)
  
