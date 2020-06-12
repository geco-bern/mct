get_bal <- function(df, varnam_bal, varnam_et, varnam_prec){
  
  if (any(is.na(df[[varnam_et]]))){
    ## linearly interpolate ET
    df[[varnam_et]] <- myapprox(df[[varnam_et]])
  }    
  
  ## calculate daily water balance
  df[[varnam_bal]] <- df[[varnam_prec]] - df[[varnam_et]]
  
  ## remove NAs at head and tail (no interpolation possible)
  if (any(is.na(df[[varnam_bal]]))){
    df <- cutna_headtail_df(df, varnam_bal)
  }
  
  return(df)
}