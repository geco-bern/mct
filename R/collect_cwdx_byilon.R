collect_cwdx_byilon <- function(ilon){
  
  dirn <- "data/df_cwdx_10_20_40/"
  filn <- paste0("df_cwdx_10_20_40_ilon_", ilon, ".RData")
  path <- paste0(dirn, filn)
  
  if (file.exists(path)) {
    print(paste("opening file", path))
    load(path)
  } else {
    rlang::inform(paste("File does not exist:", path))
    df <- NULL
  }
  
  return(df)

}
