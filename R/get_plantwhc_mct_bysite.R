get_plantwhc_mct_bysite <- function( df, 
                                     out_mct = NULL,
                                     varname_wbal = "wbal", 
                                     varname_date = "date",
                                     thresh_terminate = 0.0, 
                                     thresh_drop = 0.9,
                                     return_period = c(2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 120, 200, 250, 300, 500, 800),
                                     verbose = FALSE, 
                                     fittype = NULL,
                                     max_nyear_acc_cwd = 5){
  
  ## check if any negative water balance occurs at all
  any_negative_bal <- any(df[[ varname_wbal ]] < 0)

  if (any_negative_bal){

    failed <- FALSE
  
    if ( sum( !is.na(df[[ varname_wbal ]]) ) < 30 ){
      
      failed <- TRUE

    } else {

      ##--------------------------------
      ## Get events of consecutive water deficit and accumulated deficit
      ##--------------------------------
      out_mct <- mct(df, varname_wbal = varname_wbal, varname_date = varname_date, thresh_terminate = thresh_terminate, thresh_drop = thresh_drop )
      
      if (nrow(out_mct$inst)>0){
        ##--------------------------------
        ## get annual maximum CWD
        ##--------------------------------
        df <- df %>% 
          mutate(year = lubridate::year(time))
        
        out_mct$inst <- out_mct$inst %>% 
          ungroup() %>% 
          mutate(year = lubridate::year(date_start))
        
        ## test if cwd continues accumulating with events spanning more than one year
        while ((sum(!(unique(df$year) %in% unique(out_mct$inst$year))) > max_nyear_acc_cwd) &&  thresh_terminate < 0.8){
          
          ## if CWD accumulates over more than 'max_nyear_acc_cwd', run 'mct()' again with relaxed 'thresh_terminate'
          thresh_terminate <- thresh_terminate + 0.2
          
          out_mct <- mct(df, varname_wbal = varname_wbal, varname_date = varname_date, thresh_terminate = thresh_terminate, thresh_drop = thresh_drop )
          
          out_mct$inst <- out_mct$inst %>% 
            ungroup() %>% 
            mutate(year = lubridate::year(date_start))
          
        }
        
        vals <- out_mct$inst %>% 
          group_by(year) %>% 
          summarise(deficit = max(deficit, na.rm = TRUE), .groups = 'drop') %>%       
          pull(deficit)      
        

        if (length(vals)>3){
          
          if (!is.null(fittype)){
            ##--------------------------------
            ## Prescribed distribution: Gumbel
            ##--------------------------------
            ## Fit a specific extreme value distribution (Gumbel)
            evd <- try( extRemes::fevd(x=vals, type=fittype, method="MLE", units = "years") )
            if (class(evd) == "try-error"){ failed <- TRUE }
              
          } else {
            ##--------------------------------
            ## Free distribution: GEV or Gumbel
            ##--------------------------------
            ## Fit a general extreme value distribution, use Gumbel instead if it works better than GEV
            ## if shape not significant different from 0 when using GEV, then it's gumbel? shape parameter is significantly different from zero, hence GEV is supported
            evd_gev <- try(extRemes::fevd(x=vals, type="GEV", method="MLE", units = "years"))
            
            ## if shape not significant different from 0 when using GEV, then it's gumbel?
            evd_gumbel <- try(extRemes::fevd(x=vals, type="Gumbel", method="MLE", units = "years"))
            
            if (class(evd_gev) != "try-error" && class(evd_gumbel) != "try-error"){

              ## is GEV-fit besser als Gumbel? Gumbel ist gute Annahme da p nicht signifikant
              df_test_fevd <- lr.test(evd_gumbel, evd_gev) %>% 
                broom::tidy()
              pval <- df_test_fevd %>% 
                pull(p.value)
              ratio <- df_test_fevd %>% 
                pull(statistic)
              
              if (ratio > 1 && pval < 0.05){
                if (verbose) rlang::inform("It's a Gumbel!!!")
                evd <- evd_gumbel
              } else {
                if (verbose) rlang::inform("It's a GEV")
                evd <- evd_gev
              }
              
            } else {
              failed <- TRUE
            }
            
          }

        } else {
          failed <- TRUE
        }
        
      } else {
        failed <- TRUE
      }
      
    }

  } else {
    failed <- FALSE
  }
  
  ##--------------------------------
  ## Get magnitudes of extremes with given return period
  ##--------------------------------  
  if (failed){

    df_return <- tibble(
      return_period = return_period, 
      return_level = rep(NA, length(return_period))
      )
    evd <- NA
    out_mct <- NA

  } else if (!any_negative_bal){

    df_return <- tibble(
      return_period = return_period, 
      return_level  = rep(0, length(return_period))
      )
    evd <- NA
    df <- df %>% mutate(deficit = 0, iinst = NA, dday = NA)
    out_mct <- list(inst = inst, df = df)

  } else {
    ## Get values for return periods and constructi data frame with it
    return_level <- extRemes::return.level(
      evd, 
      return.period = return_period )
    
    df_return <- tibble( 
      return_period = return_period, 
      return_level = unname(c(return_level)))           
  }
  
  return(list(df_return = df_return, mod = evd, mct = out_mct, thresh_terminate = thresh_terminate))
}