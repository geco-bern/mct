collect_cwd_lue0_byilon <- function(ilon){
  
  ## construct output file name
  dirn <- "~/mct/data/df_cwd_lue0/"
  filn <- paste0("df_cwd_lue0_", ilon, ".RData")
  path <- paste0(dirn, filn)
  
  load(path)
  
  return(df)

}