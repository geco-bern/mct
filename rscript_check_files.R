check_avail_alexi_tidy <- function(ilon){
  
  ## written by rscript_get_data_alexi.R -> rbeni::nclist_to_df()
  dir <- "~/data/alexi_tir/data_tidy/"
  filnam <- paste0("EDAY_CERES__ilon_", ilon, ".RData")
  avl <- file.exists(paste0(dir, filnam))
  
  return(avl)
}

check_avail_et_mm <- function(ilon){
  
  ## written by R/get_et_mm_bylon.R
  dir <- "~/mct/data/df_alexi_et_mm/"
  filnam <- paste0("df_alexi_et_mm_ilon_", ilon, ".RData")
  avl <- file.exists(paste0(dir, filnam))
  
  return(avl)
}

check_avail_snow <- function(ilon){
  
  ## written by R/simulate_snow_byilon.R
  dirn <- "~/mct/data/df_snow/"
  filn <- paste0("df_snow_ilon_", ilon, ".RData")
  avl <- file.exists(paste0(dirn, filn))
  
  return(avl)
}

check_avail_bal <- function(ilon){
  
  ## written by R/get_bal_byilon.R
  dirn <- "~/mct/data/df_bal/"
  filn <- paste0("df_bal_ilon_", ilon, ".RData")
  avl <- file.exists(paste0(dirn, filn))
  
  return(avl)
}

check_avail_cwdx <- function(ilon){
  
  ## written by R/get_cwdx_byilon.R
  dirn <- "~/mct/data/df_cwdx/"
  filn <- paste0("df_cwdx_ilon_", ilon, ".RData")
  avl <- file.exists(paste0(dirn, filn))
  
  return(avl)
}

check_avail_10_20_40 <- function(ilon){
  
  ## written by R/extract_cwdx_byilon.R
  dirn <- "~/mct/data/df_cwdx_10_20_40/"
  filn <- paste0("df_cwdx_10_20_40_ilon_", ilon, ".RData")
  avl <- file.exists(paste0(dirn, filn))
  
  return(avl)
}

get_ilon_lores <- function(ilon_hires){
  
  lon_lores <- seq(-179.75, 179.75, by = 0.5)
  lon_hires <- seq(-179.975, 179.975, by = 0.05)
  ilon_lores <- which.min(abs(lon_lores - lon_hires[ilon_hires]))
  
  return(ilon_lores)
}

df <- tibble(ilon = 1:7200) %>% 
  rowwise() %>% 
  mutate(
    ilon_lores = get_ilon_lores(ilon),
    avl_tidy = check_avail_alexi_tidy(ilon),
    avl_et_mm = check_avail_et_mm(ilon),
    avl_snow = check_avail_snow(ilon),
    avl_bal = check_avail_bal(ilon),
    avl_cwdx = check_avail_cwdx(ilon),
    avl_cwdx_10_20_40 = check_avail_10_20_40(ilon)
  )

df %>% 
  filter(avl_tidy)
