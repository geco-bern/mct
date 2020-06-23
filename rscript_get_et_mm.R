source("R/get_et_mm_bylon.R")

nlon <- 3600
ncores <- 2

if (ncores > 1){
  
  cl <- multidplyr::new_cluster(ncores) %>%
    multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr")) %>%
    multidplyr::cluster_assign(get_et_mm_bylon = get_et_mm_bylon) %>%
    
    ## distribute to cores, making sure all data from a specific site is sent to the same core
    df_out <- tibble(ilon = seq(nlon)) %>%
      multidplyr::partition(cl) %>%
      dplyr::mutate(out = purrr::map_int( ilon,
                                          ~get_et_mm_byilon(.)))
    
} else {
  
  purrr::map(as.list(seq(nlon)), ~get_et_mm_byilon(.))
  
}
