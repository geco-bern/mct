get_return_period <- function(x){
  
  ## extracted parts from plot.fevd.mle.R
  xp <- ppoints(n = x$n, a = 0)
  ytmp <- datagrabber(x)
  y <- c(ytmp[, 1])
  
  return_periods <- -1/log(xp)
  
  df <- dplyr::tibble(
    xp = xp, 
    return_periods = return_periods, 
    return_values = sort(y) ) %>% 
    dplyr::mutate( trans_period = -log( -log(1 - 1/return_periods)) )

  return(df)
}