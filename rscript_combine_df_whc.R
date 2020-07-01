library(dplyr)
dir <- "~/data/mct_data/df_whc_hires_chunks2/"
filelist <- paste0(dir, list.files(dir, pattern = "df_whc_hires_chunk_.*.RData"))

list_whc_out <- list()
for (ifil in filelist){
  load(ifil)
  list_whc_out[[ifil]] <- df_whc
}
df_whc <- bind_rows(list_whc_out)

save(df_whc, file = "~/data/mct_data/df_whc_hires.RData")