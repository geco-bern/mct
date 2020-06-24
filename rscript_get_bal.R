library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(multidplyr)
library(rbeni)
source("R/get_bal_byilon.R")

nlon <- 7200
ncores <- 2

if (ncores > 1){
  
  cl <- multidplyr::new_cluster(ncores) %>%
    multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr", "rbeni")) %>%
    multidplyr::cluster_assign(get_bal_byilon = get_bal_byilon) %>%
    
    ## distribute to cores, making sure all data from a specific site is sent to the same core
    df_out <- tibble(ilon = seq(nlon)) %>%
      multidplyr::partition(cl) %>%
      dplyr::mutate(out = purrr::map_int( ilon,
                                          ~get_bal_byilon(.)))
    
} else {
  
  df_out <- purrr::map(as.list(seq(nlon)[1000]), ~get_bal_byilon(.))
  
}
