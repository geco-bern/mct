#' Cumulative water deficit
#' 
#' Derives time series of the cumulative water deficit (CWD), given time series of 
#' the daily soil water balance (infiltration minus evapotranspiration). CWD "events"
#' are identified as periods of consecutive days where the CWD is positive (a water deficit).
#' CWD is set to zero after rain has reduced the CWD by a user-defined fraction, relative to 
#' maximum CWD attained during the same event.
#'
#' @param df a data frame that contains the variable named according to argument \code{varname_wbal}
#' @param varname_wbal name of the variable representing the daily soil water balance (infiltration minus evapotranspiration)
#' @param varname_date name of the variable representing information about the date (format irrelevant)
#' @param thresh_terminate threshold determining the level, relative to maximum CWD attained 
#' during the same event, to which a CWD has to be reduced to terminate the event. Defaults to 0, 
#' meaning that the CWD has to be fully compensated by water infiltration into the soil to terminate
#' a CWD event.
#' @param thresh_drop Level, relative to the CWD maximum of the same event, after which all data 
#' during the remainder of the event is set to missing values. This is to avoid interpreting data
#' after rain events but before full compensation of CWD. Defaults to 0.9.
#'
#' @import dplyr
#' 
#' @details A list of two data frames (tibbles). \code{inst} contains information about CWD "events". 
#' Each row corresonds to one event. An event is defined as a period of consecutive days where the 
#' CWD is positive (a water deficit)) and has the following columns:
#' 
#' \code{idx_start}: row number of \code{df} of which the date corresponds to the start of the event
#' \code{len}: length of the event, quantified as number of rows in \code{df} corresponding to the event
#' \code{iinst}: event number
#' \code{date_start}: starting date of the event, formatted as \code{varname_date} in \code{df}.
#' \code{date_end}: end date of the event, formatted as \code{varname_date} in \code{df}.
#' \code{deficit}: maximum CWD recorded during this event. Units correspond to units of \code{varname_wbal} 
#' in \code{df}.
#' 
#' @export
#' 
mct <- function(df, varname_wbal, varname_date, thresh_terminate = 0.0, thresh_drop = 0.9){
  
  if (thresh_terminate > thresh_drop){
    rlang::warn("Aborting. thresh_terminate must be smaller or equal thresh_drop. Setting it equal.")
    thresh_terminate <- thresh_drop
  }
  
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
      this_inst <- tibble( idx_start = idx, len = iidx_drop-idx, iinst = iinst, date_start=df[[varname_date]][idx], date_end = df[[varname_date]][iidx_drop-1], deficit = max_deficit )
      inst <- inst %>% bind_rows(this_inst)
      
      ## update
      iinst <- iinst + 1
      dday <- 0
      idx <- iidx
    }
    
  }
  return( list(inst=inst, df=df))
}