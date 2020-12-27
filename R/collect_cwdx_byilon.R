collect_cwdx_byilon <- function(ilon){
  
  dirn <- "~/mct/data/df_cwdx_10_20_40/"
  filn <- paste0("df_cwdx_10_20_40_ilon_", ilon, ".RData")
  path <- paste0(dirn, filn)
  
  print(paste("opening file", path))

  load(path)
  
  return(df)

}