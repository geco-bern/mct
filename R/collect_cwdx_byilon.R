collect_cwdx_byilon <- function(ilon, config = read_input_config()){
  
  path <- climate_output_path(
    paste0("data/df_cwdx_10_20_40/df_cwdx_10_20_40_ilon_", ilon, ".RData"),
    config
  )
  
  if (file.exists(path)) {
    print(paste("opening file", path))
    load(path)
  } else {
    rlang::inform(paste("File does not exist:", path))
    df <- NULL
  }
  
  return(df)

}
