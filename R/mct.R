mct <- function(df, varname_wbal, method="threshbal", thresh_deficit=0.5){
  
  inst <- tibble()
  idx <- 0
  df$deficit <- rep(0, nrow(df))
  
  ## search all dates
  while (idx <= (nrow(df)-1)){
    idx <- idx + 1
    
    ## if the water balance (deficit = prec - pet*fv) is negative, start accumulating deficit
    if (df[[ varname_wbal ]][idx]<0){
      
      deficit <- 0
      if (method=="threshbal") max_deficit <- 0
      iidx <- idx
      
      if (method=="posbal"){
        ## continue accumulating deficit as long as the water balance is negative
        while (iidx <= (nrow(df)-1) && df[[ varname_wbal ]][iidx]<0){
          deficit <- deficit - df[[ varname_wbal ]][iidx]
          df$deficit[iidx] <- deficit
          iidx <- iidx + 1
        }
        
      } else if (method=="threshbal"){
        ## continue accumulating deficit as long as the deficit is not recuded by more than (thresh_deficit*100) % 
        while (iidx <= (nrow(df)-1) && (deficit - df[[ varname_wbal ]][iidx] > (1-thresh_deficit) * max_deficit)){
          deficit <- deficit - df[[ varname_wbal ]][iidx]
          if (deficit>max_deficit) max_deficit <- deficit
          df$deficit[iidx] <- deficit
          iidx <- iidx + 1
        }
      }
      
      ## record instance
      this_inst <- tibble( date_start=df$date[idx], date_end=df$date[iidx-1], deficit=deficit, len=iidx-idx )
      inst <- inst %>% bind_rows(this_inst)
      
      ## update
      idx <- iidx
    }
  }
  return( list(inst=inst, df=df))
}