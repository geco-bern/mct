collect_cwd_et0_byilon <- function(ilon){
  
  ## construct output file name
  dirn <- "~/mct/data/df_cwd_et0/"
  filn <- paste0("df_cwd_et0_", ilon, ".RData")
  path <- paste0(dirn, filn)
  
  load(path)
  
  return(df)

}