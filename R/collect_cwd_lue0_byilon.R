collect_cwd_lue0_byilon <- function(ilon, config = read_input_config()){
  
  ## construct output file name
  path <- climate_output_path(
    paste0("data/df_cwd_lue0_2/df_cwd_lue0_", ilon, ".RData"),
    config
  )
  
  if (file.exists(path)){
    load(path)
  } else {
    rlang::inform(paste("File does not exist:", path))
    df <- NULL
  }
  
  return(df)

}
