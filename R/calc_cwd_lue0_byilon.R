calc_cwd_lue0_byilon <- function(ilon){
  
  source("R/calc_cwd_lue0.R")
  
  ## construct output file name
  dirn <- "~/mct/data/df_cwd_lue0/"
  filn <- paste0("df_cwd_lue0_", ilon, ".RData")
  if (!dir.exists(dirn)) system("mkdir -p ~/mct/data/df_cwd_lue0")
  path <- paste0(dirn, filn)
  
  ## Open file CWDX output
  dirn <- "~/mct/data/df_cwdx/"
  filn <- paste0("df_cwdx_ilon_", ilon, ".RData")
  load(paste0(dirn, filn)) # loads 'df'
  
  ## extract data from CWDX output. This now contains the CWD and instances information
  df_cwd <- df %>%
    mutate(mct = purrr::map(out_mct, "mct")) %>% 
    dplyr::select(-out_mct) %>% 
    mutate(data_cwd = purrr::map(mct, "df")) %>% 
    dplyr::select(-mct)
  
  ## Load SiF data
  ## version PK
  filn <- paste0("GOME_PK_dcSIF_005deg_8day__ilon_", ilon, ".RData")
  dirn <- "~/data/gome_2_sif_downscaled/data_tidy/"
  load(paste0(dirn, filn)) # loads 'df'
  df_pk <- df %>% 
    rename(data_pk = data)
  
  ## version JJ
  filn <- paste0("GOME_JJ_dcSIF_005deg_8day__ilon_", ilon, ".RData")
  dirn <- "~/data/gome_2_sif_downscaled/data_tidy/"
  load(paste0(dirn, filn)) # loads 'df'
  
  test <- df %>% 
    
    ## Take mean of two versions of SIF data
    rename(data_jj = data) %>% 
    full_join(df_pk, by = c("lon", "lat")) %>% 
    mutate(data = purrr::map2(data_jj, data_pk, ~bind_rows(.x, .y))) %>% 
    dplyr::select(-data_jj, -data_pk) %>% 
    mutate(data = purrr::map(data, ~group_by(., time) %>% summarise(SIF = mean(SIF, na.rm = TRUE), .groups = "drop"))) %>% 

    ## Combine with CWD data
    inner_join(df_cwd, by = c("lon", "lat")) %>% 
    mutate(data = purrr::map2(data_cwd, data, ~left_join(.x, .y, by = "time"))) %>% 
    dplyr::select(lon, lat, data)
  
  
  
  ## testing
  df_cwd_lat <- df_cwd$lat %>% unique()
  df_sif_lat <- df$lat %>% unique()
  df_com_lat <- test$lat %>% unique()
  
    pull(lon)
  
    mutate(cwdx20 = purrr::map_dbl(out_mct, ~extract_return_level(., 20))) %>% 
    mutate(mct = purrr::map(out_mct, "mct")) %>% 
    ungroup() %>% 
    dplyr::select(idx, mct, cwdx20) %>% 
    mutate(df = purrr::map(mct, "df"),
           inst = purrr::map(mct, "inst")) %>% 
    dplyr::select(-mct) %>% 
    
    ## add sif data
    left_join(
      df_sif_jj %>% dplyr::select(idx, df_jj = data),
      by = "idx"
    ) %>% 
    left_join(
      df_sif_pk %>% dplyr::select(idx, df_pk = data),
      by = "idx"
    ) %>% 
    dplyr::filter(!is.null(df) & !is.null(df_jj) & !is.null(df_pk) & !is.na(cwdx20)) %>% 
    mutate(df = purrr::map2(df, df_jj, ~left_join(.x, .y, by = "date"))) %>% 
    mutate(df = purrr::map(df, ~rename(., sif_jj = sif))) %>% 
    mutate(df = purrr::map2(df, df_pk, ~left_join(.x, .y, by = "date"))) %>% 
    mutate(df = purrr::map(df, ~rename(., sif_pk = sif))) %>% 
    # mutate(df = purrr::map(df, ~add_mean_sif(.))) %>% 
    mutate(df = purrr::map(df, ~rename(., lue = sif_jj))) %>%
    
    ## get CWD at LUE0=0
    mutate(out_lue0 = purrr::map2(df, inst, ~calc_cwd_lue0(.x, .y, do_plot = TRUE))) %>% 
    mutate(cwd_lue0 = purrr::map_dbl(out_lue0, "lue0")) %>% 
    mutate(cwd_lue0_exp = purrr::map_dbl(out_lue0, "lue0_exp")) %>% 
    mutate(gg = purrr::map(out_lue0, "gg")) %>% 
    
    ## get fLUE at CWDmax
    mutate(out_flue = purrr::map(df, ~get_flue_cwdmax(.))) %>% 
    mutate(flue = purrr::map_dbl(out_flue, "flue")) %>% 
    mutate(cwdmax = purrr::map_dbl(out_flue, "cwdmax")) %>% 
    
    dplyr::select(idx, cwdx20, cwd_lue0, cwd_lue0_exp, gg, flue, cwdmax)
  
  
  
}