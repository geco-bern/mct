get_data_fluxnet2015_mct <- function(sitename){
  
  require(dplyr)
  
  ## get data from my own collection
  load("./data/ddf_fluxnet2015.RData")
  df <- ddf %>% 
    dplyr::as_tibble() %>% 
    dplyr::filter(sitename==!!sitename) %>% 
    dplyr::select(date, fv=fapar, prec=prec_fluxnet2015)
  
  ## add PET from the model output
  load("./data/list_ddf_mod.RData")
  df <- df %>% 
    dplyr::left_join( dplyr::select( list_ddf_mod[[sitename]], date, pet ), by="date" )
  
  return(df)
}