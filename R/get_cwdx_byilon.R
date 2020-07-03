get_cwdx_byilon <- function(ilon_hires){
  
  source("R/mct2.R")
  source("R/get_plantwhc_mct_bysite.R")
  
  dirn <- "~/mct/data/df_cwdx/"
  filn <- paste0("df_cwdx_ilon_", ilon_hires, ".RData")
  if (!dir.exists(dirn)) system("mkdir -p ~/mct/data/df_cwdx")
  path <- paste0(dirn, filn)

  if (!file.exists(path)){
  
    ## Open file with daily water balance
    dirn <- "~/mct/data/df_bal/"
    filn <- paste0("df_bal_ilon_", ilon_hires, ".RData")
    load(paste0(dirn, filn)) # loads 'df'
    
    ## determine CWD and events
    df <- df %>% 
      
      dplyr::mutate(
        out_mct = purrr::map(
          data,
          ~get_plantwhc_mct_bysite(
            .,
            varname_wbal = "bal",
            varname_date = "time",
            thresh_terminate = 0.0,
            thresh_drop = 0.9,
            fittype = "Gumbel"))
      ) %>% 
      dplyr::select(-data)

    print(paste("Writing file:", path))
    save(df, file = path)
    
  } else {
    print(paste("File exists already:", path))
  } 
  
  error = 0
  return(error)
}