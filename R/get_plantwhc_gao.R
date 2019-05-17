get_plantwhc_mct <- function( df, coln_prec="prec", coln_pet="pet", 
                      coln_fv="fv", settings=list(method_mct="threshbal", thresh_deficit=0.5),
                      return_period=c(2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 120, 200, 250, 300, 500, 800) ){

  ## Get data frame in shape (renaming columns)
  df <- df %>%
    dplyr::rename(prec=!!coln_prec, pet=!!coln_pet, fv=!!coln_fv) %>% 

    ## clean: fill gaps by linearly interpolating
    mutate(
      fv = rbeni::myapprox(fv), 
      pet = rbeni::myapprox(pet), 
      prec=ifelse(is.na(prec),0,prec)) %>% 
    
    ## Get daily water balance and interpolate missing values
    mutate(bal = prec - fv*pet) %>% 
    mutate(bal = rbeni::myapprox(bal)) %>% 
  
    ## drop NAs at head and tail
    rbeni::cutna_headtail_df("fv")
  
  ## Get events of consecutive water deficit and accumulated deficit
  source("R/mct.R")
  out <- mct(df, method = "threshbal", thresh_deficit=0.5)
  
  ## Get N largest deficts
  ## Take only the N largest instances (deficits), where N is given by the number of years available in the data
  nyears <- year(range(out$df$date)[2]) - year(range(out$df$date)[1]) + 1
  vals <- out$inst %>% 
    arrange(desc(deficit)) %>% 
    dplyr::slice(1:nyears) %>% 
    select(deficit) %>% 
    unlist() %>% 
    unname()
  
  ## Fit an extreme value distribution (Gumbel)
  gumbi <- extRemes::fevd(x=vals, type="Gumbel", method="MLE")
  
  ## Get values for return periods and constructi data frame with it
  return_level <- extRemes::return.level(
    gumbi, 
    return.period = return_period )
  
  df_return <- tibble( 
    return_period = return_period, 
    return_level = unname(c(return_level)), 
    trans_period = -log( -log(1 - 1/return_period)) )
  
  return(df_return)
}


