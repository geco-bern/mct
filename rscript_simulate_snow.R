library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(multidplyr)
source("R/simulate_snow_byilon.R")

nlon <- 720
ncores <-32

if (ncores > 1){
  
  cl <- multidplyr::new_cluster(ncores) %>%
    multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr")) %>%
    multidplyr::cluster_assign(simulate_snow_byilon = simulate_snow_byilon)
    
  ## distribute to cores, making sure all data from a specific site is sent to the same core
  df_out <- tibble(ilon = seq(nlon)) %>%
    multidplyr::partition(cl) %>%
    dplyr::mutate(out = purrr::map( ilon,
                                    ~simulate_snow_byilon(.)))
    
} else {
  
  df_out <- purrr::map(as.list(seq(nlon)[1000]), ~simulate_snow_byilon(.))
  
}
