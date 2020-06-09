mct <- function(df, varname_wbal, thresh_terminate = 0.0, thresh_drop = 0.9){
  
  if (thresh_terminate > thresh_drop) rlang::abort("Aborting. thresh_terminate must be smaller or equal thresh_drop.")
  
  inst <- tibble()
  idx <- 0
  iinst <- 1
  
  df <- df %>% 
    ungroup() %>% 
    # dplyr::select(date, !!varname_wbal) %>% 
    mutate(iinst = NA, dday = NA, deficit = 0)
  
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
      done_finding_dropday <- FALSE

      # ## xxx debug
      # newlargeinst <- TRUE
      
      ## continue accumulating deficit as long as the deficit has not fallen below (thresh_terminate) times the maximum deficit attained in this event
      while (iidx <= (nrow(df)-1) && (deficit - df[[ varname_wbal ]][iidx] > thresh_terminate * max_deficit)){
        dday <- dday + 1
        deficit <- deficit - df[[ varname_wbal ]][iidx]
        
        # ## xxx debug
        # if (newlargeinst && deficit>50){
        #   print("entering large deficit...")
        #   newlargeinst <- FALSE
        # }
        
        ## record the maximum deficit attained in this event
        if (deficit > max_deficit){
          max_deficit <- deficit
          done_finding_dropday <- FALSE
        } 
        
        ## record the day when deficit falls below (thresh_drop) times the current maximum deficit
        if (deficit < (max_deficit * thresh_drop) && !done_finding_dropday){
          iidx_drop <- iidx
          done_finding_dropday <- TRUE
        }

        ## once, deficit has fallen below threshold, all subsequent dates are dropped (dday set to NA)
        if (done_finding_dropday){
          df$iinst[iidx] <- NA
          df$dday[iidx]  <- NA
        } else {
          df$iinst[iidx] <- iinst
          df$dday[iidx]  <- dday
          iidx_drop <- iidx
        }

        # ## (even before "drop-day" is found), drop data of days after rain, i.e., where current CWD is below the maximum (previously) attained in the same event
        # if (deficit < max_deficit){
        #   df$iinst[iidx] <- NA
        #   df$dday[iidx]  <- NA
        #   df$deficit[iidx] <- NA
        # } else {
        #   df$iinst[iidx] <- iinst
        #   df$dday[iidx]  <- dday
        #   df$deficit[iidx] <- deficit
        # }

        df$deficit[iidx] <- deficit
        
        iidx <- iidx + 1
        
      }
      
      # ## xxx debug
      # if (!newlargeinst){
      #   print("crucial moment")
      # }
      
      ## record instance
      this_inst <- tibble( idx_start = idx, len = iidx_drop-idx, iinst = iinst, date_start=df$date[idx], date_end = df$date[iidx_drop-1], deficit = max_deficit )
      inst <- inst %>% bind_rows(this_inst)
      
      ## update
      iinst <- iinst + 1
      dday <- 0
      idx <- iidx
    }
    
  }
  return( list(inst=inst, df=df))
}