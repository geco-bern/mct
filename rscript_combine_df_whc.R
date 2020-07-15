library(dplyr)
library(tidyr)

dir <- "~/data/mct_data/df_whc_hires_chunks/"
filelist <- paste0(dir, list.files(dir, pattern = "df_whc_hires_chunk_.*.RData"))

list_whc_out <- list()
for (ifil in filelist){
  load(ifil)
  list_whc_out[[ifil]] <- df_whc %>% 
    ungroup() %>% 
    unnest(data_soiltext_top) %>% 
    dplyr::select(lon, lat, fc_top = fc, pwp_top = pwp, whc_top = whc, data_soiltext_sub) %>% 
    unnest(data_soiltext_sub) %>% 
    dplyr::select(lon, lat, fc_top, pwp_top, whc_top, fc_sub = fc, pwp_sub = pwp, whc_sub = whc)
}
df_whc <- bind_rows(list_whc_out)

save(df_whc, file = "~/data/mct_data/df_whc_hires.RData")