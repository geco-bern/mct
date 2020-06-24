library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(multidplyr)
source("R/get_cwdx_byilon.R")

nlon <- 7200
ncores <- 2

if (ncores > 1){
  
  cl <- multidplyr::new_cluster(ncores) %>%
    multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr")) %>%
    multidplyr::cluster_assign(get_cwdx_byilon = get_cwdx_byilon) %>%
    
    ## distribute to cores, making sure all data from a specific site is sent to the same core
    df_out <- tibble(ilon = seq(nlon)) %>%
      multidplyr::partition(cl) %>%
      dplyr::mutate(out = purrr::map_int( ilon,
                                          ~get_cwdx_byilon(.)))
    
} else {
  
  ## testing
  df_out <- purrr::map(as.list(seq(nlon)[1000]), ~get_cwdx_byilon(.))
  
}
