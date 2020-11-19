get_cwdx_byilon <- function(ilon_hires, df_lat = NULL){
  
  source("R/mct2.R")
  source("R/get_plantwhc_mct_bysite.R")
  
  ## construct output file name
  dirn <- "~/mct/data/df_cwdx/"
  filn <- paste0("df_cwdx_ilon_", ilon_hires, ".RData")
  if (!dir.exists(dirn)) system("mkdir -p ~/mct/data/df_cwdx")
  path <- paste0(dirn, filn)
  
  if (!is.null(df_lat)){
    ##-----------------------
    ## for redo failed only
    ##-----------------------
    print(paste("Complementing file:", path))
    if (!file.exists(path)) rlang::abort(paste("Aborting. File does not exist:", path))
    load(path)
    out_cwdx <- df
    rm("df")

    ## Open file with daily water balance
    dirn <- "~/mct/data/df_bal/"
    filn <- paste0("df_bal_ilon_", ilon_hires, ".RData")
    if (!file.exists(paste0(dirn, filn))) rlang::abort(paste("Aborting. File does not exist:", paste0(dirn, filn)))
    load(paste0(dirn, filn)) # loads 'df'
    
    ## do it only for latitudes (single cells) where CWDX data was actually found missing
    out_cwdx_sub <- df %>% 
      dplyr::filter(lat %in% pull(df_lat, lat)) %>% 
      
      ## this runs the function with newly implemented incremental threshhold_drop search 
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
    
    # ## test
    # out_cwdx_sub$out_mct[[1]]$df_return
    # tmp <- out_cwdx %>%
    #   dplyr::filter(lon == out_cwdx_sub$lon[[1]] & lat == out_cwdx_sub$lat[[1]])
    # tmp$out_mct[[1]]$df_return
    
    ## overwrite respective column in previous one (warning: for some reason the ifelse only retains df_return)
    out_cwdx$out_mct_save <- out_cwdx$out_mct
    for (idx in seq(nrow(out_cwdx_sub))){
      idx_out_cwdx <- which(out_cwdx$lon == out_cwdx_sub$lon[idx] & out_cwdx$lat == out_cwdx_sub$lat[idx])
      out_cwdx$out_mct[idx_out_cwdx] <- out_cwdx_sub$out_mct[idx]
    }
    
    ## this code doesn't work for some reason:
    # out_cwdx <- out_cwdx %>% 
    #   left_join(out_cwdx_sub %>% rename(out_mct_redo = out_mct), by = c("lon", "lat")) %>%
    #   rowwise() %>% 
    #   ungroup() %>% 
    #   mutate(avl_redo = purrr::map_lgl(out_mct_redo, ~!is.null(.))) %>% 
    #   mutate(out_mct_save = out_mct) %>% 
    #   rowwise() %>% 
    #   mutate(out_mct = ifelse(avl_redo, out_mct_redo, out_mct)) %>% 
    #   dplyr::select(-avl_redo, -out_mct_redo)
    
    ## write to file
    df <- out_cwdx
    rm("out_cwdx")
    print(paste("Writing file:", path))
    save(df, file = path)
    
  } else {
    
    if (!file.exists(path)){
      
      ## Open file with daily water balance
      dirn <- "~/mct/data/df_bal/"
      filn <- paste0("df_bal_ilon_", ilon_hires, ".RData")
      load(paste0(dirn, filn)) # loads 'df'
      
      ## determine CWD and events
      df <- df %>% 
        
        # ## xxx test
        # ungroup() %>% 
        # slice(110:120) %>% 
        
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
    
  }

  error = 0
  return(error)
}