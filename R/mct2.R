mct <- function(df, varname_wbal, thresh_terminate = 0.2, thresh_drop = 0.8){
  
  if (thresh_terminate > thresh_drop) rlang::abort("Aborting. thresh_terminate must be smaller or equal thresh_drop.")
  
  inst <- tibble()
  idx <- 0
  iinst <- 1
  
  df$deficit <- rep(0, nrow(df))
  df$iinst   <- rep(NA, nrow(df))
  df$dday    <- rep(NA, nrow(df))
  
  ## search all dates
  while (idx <= (nrow(df)-1)){
    idx <- idx + 1
    
    ## if the water balance (deficit = prec - et) is negative, start accumulating deficit
    ## cumulate negative water balances (deficits)
    if (df[[ varname_wbal ]][idx]<0){
      
      dday <- 0
      deficit <- 0
      max_deficit <- 0
      iidx <- idx
      iidx_drop <- iidx
      done_recording_dropday <- FALSE
      
      ## continue accumulating deficit as long as the deficit has not fallen below (thresh_terminate) times the maximum deficit attained in this event
      while (iidx <= (nrow(df)-1) && (deficit - df[[ varname_wbal ]][iidx] > thresh_terminate * max_deficit)){
        dday <- dday + 1
        deficit <- deficit - df[[ varname_wbal ]][iidx]
        
        ## record the maximum deficit attained in this event
        if (deficit > max_deficit) max_deficit <- deficit
        
        ## record the day when deficit falls below (thresh_drop) times the maximum deficit
        if (deficit < max_deficit * thresh_drop && !done_recording_dropday){
          iidx_drop <- iidx
          done_recording_dropday <- TRUE
        }
        
        if (done_recording_dropday){
          df$deficit[iidx] <- NA
          df$iinst[iidx] <- NA
          df$dday[iidx]  <- NA
        } else {
          df$deficit[iidx] <- deficit
          df$iinst[iidx] <- iinst
          df$dday[iidx]  <- dday
        }
          
        iidx <- iidx + 1
      }
      
      ## record instance
      this_inst <- tibble( idx_start = idx, len=iidx_drop-idx, iinst = iinst, date_start=df$date[idx], date_end=df$date[iidx_drop-1], deficit=max_deficit )
      inst <- inst %>% bind_rows(this_inst)
      
      ## update
      iinst <- iinst + 1
      dday <- 0
      idx <- iidx
    }
    
  }
  return( list(inst=inst, df=df))
}