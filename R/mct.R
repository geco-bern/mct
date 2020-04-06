mct <- function(df, varname_wbal, do_deficit = TRUE, method = "threshbal", isohydric = FALSE, thresh_deficit=0.5, thresh_surplus = 0.5){
  
  inst <- tibble()
  idx <- 0
  iinst <- 1
  
  if (do_deficit){
    df$deficit <- rep(0, nrow(df))
    df$iinst   <- rep(NA, nrow(df))
    df$dday    <- rep(NA, nrow(df))
  } else {
    df$surplus <- rep(0, nrow(df))
    df$iinst   <- rep(NA, nrow(df))
    df$dday    <- rep(NA, nrow(df))
  }
  
  ## search all dates
  while (idx <= (nrow(df)-1)){
    idx <- idx + 1
    
    ## if the water balance (deficit = prec - et) is negative, start accumulating deficit
    if (do_deficit){
      ## cumulate negative water balances (deficits)
      if (df[[ varname_wbal ]][idx]<0){
        
        dday <- 0
        deficit <- 0
        max_deficit <- 0
        iidx <- idx
        
        if (method=="posbal"){
          ## continue accumulating deficit as long as the water balance is negative
          while (iidx <= (nrow(df)-1) && df[[ varname_wbal ]][iidx]<0){
            dday <- dday + 1
            deficit <- deficit - df[[ varname_wbal ]][iidx]
            if (deficit>max_deficit) max_deficit <- deficit
            df$deficit[iidx] <- deficit
            df$iinst[iidx] <- iinst
            df$dday[iidx]  <- dday
            iidx <- iidx + 1
          }
          
        } else if (method=="threshbal"){
          ## continue accumulating deficit as long as the deficit is not recuded by more than (thresh_deficit*100) % 
          while (iidx <= (nrow(df)-1) && (deficit - df[[ varname_wbal ]][iidx] > (1-thresh_deficit) * max_deficit)){
            dday <- dday + 1
            deficit <- deficit - df[[ varname_wbal ]][iidx]
            if (deficit>max_deficit) max_deficit <- deficit
            df$deficit[iidx] <- deficit
            df$iinst[iidx] <- iinst
            df$dday[iidx]  <- dday
            iidx <- iidx + 1
          }
        }
        
        ## record instance
        this_inst <- tibble( idx_start = idx, len=iidx-idx, iinst = iinst, date_start=df$date[idx], date_end=df$date[iidx-1], deficit=max_deficit )
        inst <- inst %>% bind_rows(this_inst)
        iinst <- iinst + 1
        dday <- 0
        
        ## update
        idx <- iidx
      }
      
    } else {
      ## cumulate positive water balances (surplusses)
      if (df[[ varname_wbal ]][idx]>0){
        
        surplus <- 0
        if (method=="threshbal") max_surplus <- 0
        iidx <- idx
        
        if (method=="posbal"){
          ## continue accumulating surplus as long as the water balance is negative
          while (iidx <= (nrow(df)-1) && df[[ varname_wbal ]][iidx]<0){
            surplus <- surplus + df[[ varname_wbal ]][iidx]
            df$surplus[iidx] <- surplus
            iidx <- iidx + 1
          }
          
        } else if (method=="threshbal"){
          ## continue accumulating surplus as long as the surplus is not recuded by more than (thresh_surplus*100) % 
          while (iidx <= (nrow(df)-1) && (surplus + df[[ varname_wbal ]][iidx] > (1-thresh_surplus) * max_surplus)){
            if (isohydric){
              surplus <- surplus + df[[ varname_wbal ]][iidx]
            } else {
              surplus <- surplus + df[[ varname_wbal ]][iidx]
            }
            if (surplus>max_surplus) max_surplus <- surplus
            df$surplus[iidx] <- surplus
            iidx <- iidx + 1
          }
        }
        
        ## record instance
        this_inst <- tibble( date_start=df$date[idx], date_end=df$date[iidx-1], surplus=surplus, len=iidx-idx )
        inst <- inst %>% bind_rows(this_inst)
        
        ## update
        idx <- iidx
      }
    }
  }
  return( list(inst=inst, df=df))
}