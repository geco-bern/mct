calc_zroot <- function(deficit, whc_t, whc_s){
  
  ## In whc_t and whc_s refer to plant available WHC in topsoil and subsoil.
  ## WHC is calculated based on HWSD data (https://daac.ornl.gov/SOILS/guides/HWSD.html).
  ## Described as "Topsoil refers to soil between 0 and 30 cm and subsoil to soil between 30 and 100 cm."
  zroot_t <- deficit / whc_t
  zroot_t_eff <- min(300, zroot_t)
  zroot_t_remaining <- zroot_t - zroot_t_eff
  zroot_s <- ifelse(zroot_t_remaining > 0, (zroot_t_remaining * whc_t) / whc_s, 0)
  
  # if (zroot_t_remaining > 0){
  #   deficit_s_remaining <- zroot_t_remaining * whc_t
  #   zroot_s <- deficit_s_remaining / whc_s
  # } else {
  #   zroot_s <- 0
  # }
  
  zroot <- zroot_t + zroot_s
  return(zroot)
}