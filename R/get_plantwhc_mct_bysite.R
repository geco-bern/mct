get_plantwhc_mct_bysite <- function( df, varname_wbal = "wbal", method_mct = "threshbal", thresh_deficit = 1.0,
  return_period=c(2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 120, 200, 250, 300, 500, 800) ){

  # ## Get data frame in shape (renaming columns)
  # df <- df %>%
  #   dplyr::rename(prec=!!coln_prec, pet=!!coln_pet, fv=!!coln_fv) %>% 
  # 
  #   ## clean: fill gaps by linearly interpolating
  #   mutate(
  #     fv = rbeni::myapprox(fv), 
  #     pet = rbeni::myapprox(pet), 
  #     prec=ifelse(is.na(prec),0,prec)) %>% 
  #   
  #   ## Get daily water balance and interpolate missing values
  #   mutate(wbal = prec - fv*pet) %>% 
  #   mutate(wbal = rbeni::myapprox(wbal)) %>% 
  # 
  #   ## drop NAs at head and tail
  #   rbeni::cutna_headtail_df("fv")
  
  failed <- FALSE
  if ( sum( !is.na(df[[ varname_wbal ]]) ) < 30 ){
    
    failed <- TRUE

  } else {

    ## Get events of consecutive water deficit and accumulated deficit
    out <- mct(df, varname_wbal = varname_wbal, method = method_mct, thresh_deficit = thresh_deficit)
    
    if (nrow(out$inst)>0){
      ## Get N largest deficts
      ## Take only the N largest instances (deficits), where N is given by the number of years available in the data
      nyears <- lubridate::year(range(out$df$date)[2]) - lubridate::year(range(out$df$date)[1]) + 1
      vals <- out$inst %>% 
        dplyr::arrange(desc(deficit)) %>% 
        dplyr::slice(1:nyears) %>% 
        dplyr::select(deficit) %>% 
        unlist() %>% 
        unname()
      
      if (length(vals)>3){
        
        ## Fit an extreme value distribution (Gumbel)
        gumbi <- try(
          extRemes::fevd(x=vals, type="Gumbel", method="MLE")
        )
        
        if (class(gumbi) != "try-error"){
          ## Get values for return periods and constructi data frame with it
          return_level <- extRemes::return.level(
            gumbi, 
            return.period = return_period )
          
          df_return <- tibble( 
            return_period = return_period, 
            return_level = unname(c(return_level))
          ) 
        } else {
          failed <- TRUE
        }
        
        
      } else {
        
        df_return <- tibble(
          return_period = return_period, 
          return_level = rep(NA, length(return_period))
        )
        
      }
      
      # trans_period = -log( -log(1 - 1/return_period)) )
      
    } else {
      failed <- TRUE
    }
    
  }
  
  
  if (failed){

    df_return <- tibble(
      return_period = return_period, 
      return_level = rep(NA, length(return_period))
      )

  }
  
  return(df_return)
}


