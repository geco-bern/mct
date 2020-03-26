get_return_period <- function(x){
  
  ## extracted parts from plot.fevd.mle.R
  xp <- ppoints(n = x$n, a = 0)   # rperiods = c(2, 5, 10, 20, 50, 80, 100, 120, 200, 250, 300, 500, 800)
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