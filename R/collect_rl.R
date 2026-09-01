collect_rl_nSIF <- function(ichunk, config = read_input_config()){
  
  ## construct output file name
  dirn <- "data/df_rl/"
  filn <- paste0("df_rl_nSIF_ichunk_", ichunk, "_30.RData")
  path <- climate_output_path(paste0(dirn, filn), config)
  
  if (file.exists(path)){
    load(path)
  } else {
    rlang::inform(paste("File does not exist:", path))
    df <- tibble()
  }
  
  return(df)
}

collect_rl_fet <- function(ichunk, config = read_input_config()){
  
  ## construct output file name
  dirn <- "data/df_rl/"
  filn <- paste0("df_rl_fet_ichunk_", ichunk, "_30.RData")
  path <- climate_output_path(paste0(dirn, filn), config)
  
  if (file.exists(path)){
    load(path)
  } else {
    rlang::inform(paste("File does not exist:", path))
    df <- tibble()
  }
  
  return(df)
}
