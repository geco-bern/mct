extract_cwdx_byilon <- function(ilon){
  
  load(paste0("data/df_cwdx/df_cwdx_ilon_", ilon, ".RData"))
  
  dirn <- "~/mct/data/df_cwdx_10_20_40/"
  filn <- paste0("df_cwdx_10_20_40_ilon_", ilon, ".RData")
  if (!dir.exists(dirn)) system("mkdir -p ~/mct/data/df_cwdx_10_20_40")
  path <- paste0(dirn, filn)

  if (!file.exists(path)){

    df <- df %>%
      dplyr::mutate(df_return = purrr::map(out_mct, "df_return")) %>%
      dplyr::mutate(cwdx10 = purrr::map_dbl(df_return, ~dplyr::filter(., return_period==10) %>% dplyr::pull(return_level)),
                    cwdx20 = purrr::map_dbl(df_return, ~dplyr::filter(., return_period==20) %>% dplyr::pull(return_level)),
                    cwdx40 = purrr::map_dbl(df_return, ~dplyr::filter(., return_period==40) %>% dplyr::pull(return_level))
      ) %>%
      dplyr::select(lon, lat, cwdx10, cwdx20, cwdx40)

    print(paste("Writing file:", path))
    save(df, file = path)

  } else {
    print(paste("File exists already:", path))
  }
}