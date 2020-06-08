clean_bal <- function(df, varnam_bal, varnam_et, varnam_prec){
  
  ## remove NAs at head and tail      
  df <- rbeni::cutna_headtail_df(df, varnam_bal)
  
  if (any(is.na(df[varnam_bal]))){
    ## linearly interpolate ET
    df[[varnam_et]] <- myapprox(df[[varnam_et]])
    
    ## re-calculate daily water balance
    df[[varnam_bal]] <- df[[varnam_prec]] - df[[varnam_et]]
  }
  
  return(df)
}