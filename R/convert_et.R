## Adopted from SPLASH
convert_et <- function(et_e, tc, elv){ 
  
  par_splash <- list(
		kTkelvin = 273.15,  # freezing point in K (= 0 deg C) 
		kTo = 298.15,       # base temperature, K (from P-model)
		kR  = 8.31446262,   # universal gas constant, J/mol/K (Allen, 1973)
		kMv = 18.02,        # molecular weight of water vapor, g/mol (Tsilingiris, 2008)
		kMa = 28.963,       # molecular weight of dry air, g/mol (Tsilingiris, 2008) XXX this was in SPLASH (WITH 1E-3 IN EQUATION) XXX
		kfFEC = 2.04,       # from flux to energy conversion, umol/J (Meek et al., 1984)
		kPo = 101325,       # standard atmosphere, Pa (Allen, 1973)
		kL  = 0.0065,       # temperature lapse rate, K/m (Cavcar, 2000)
		kG  = 9.80665,      # gravitational acceleration, m/s^2 (Allen, 1973)
		k_karman = 0.41,    # Von Karman constant; from bigleaf R package
		eps = 9.999e-6,     # numerical imprecision allowed in mass conservation tests
		cp = 1004.834,      # specific heat of air for constant pressure (J K-1 kg-1); from bigleaf R package
		Rd = 287.0586       # gas constant of dry air (J kg-1 K-1) (Foken 2008 p. 245; from bigleaf R package)
		)

  sat_slope <- calc_sat_slope(tc)
  lv <- calc_enthalpy_vap(tc)
  pw <- calc_density_h2o(tc, calc_patm(elv, par_splash))
  gamma <- calc_psychro(tc, calc_patm(elv, par_splash), par_splash)
  econ <- sat_slope / (lv * pw * (sat_slope + gamma))
  
  ## J m-2 d-1 -> mm / d
  et_mm <- et_e * econ * 1000

  return(et_mm)
  
}


calc_patm <- function( elv, par ){
  #----------------------------------------------------------------   
  # Calculates atmospheric pressure for a given elevation, assuming
  # standard atmosphere at sea level (kPo)
  # Ref:      Allen et al. (1998)
  # This function is copied from SPLASH
  #----------------------------------------------------------------
  # use md_params_core, only: kPo, kL, kTo, kG, kMa, kR

  # # arguments
  # real, intent(in) :: elv # elevation above sea level, m

  # # function return value
  # real ::  patm ! atmospheric pressure (Pa)

  patm <- par$kPo * (1.0 - par$kL * elv / par$kTo) ^ (par$kG * par$kMa * 1.e-3 / (par$kR * par$kL))

  return(patm)

}


calc_sat_slope <- function( tc ){
  #----------------------------------------------------------------   
  # Calculates the slope of the sat pressure temp curve, Pa/K
  # Ref:      Eq. 13, Allen et al. (1998)
  #----------------------------------------------------------------   
  # # arguments
  # real, intent(in) :: tc # air temperature, degrees C

  # # function return value
  # real :: sat_slope  # slope of the sat pressure temp curve, Pa/K

  sat_slope <- (17.269)*(237.3)*(610.78)*(exp(tc*17.269/(tc + 237.3))/((tc + 237.3)^2))

	return( sat_slope )

}


calc_enthalpy_vap <- function( tc ){
  #----------------------------------------------------------------   
  # Calculates the enthalpy of vaporization, J/kg
  # Ref:      Eq. 8, Henderson-Sellers (1984)
  #----------------------------------------------------------------   
  # # arguments
  # real, intent(in) :: tc # air temperature, degrees C

  # # function return value
  # real ::  enthalpy_vap # enthalpy of vaporization, J/kg

  enthalpy_vap <- 1.91846e6*((tc + 273.15)/(tc + 273.15 - 33.91))^2

  return( enthalpy_vap )

}


calc_density_h2o <- function( tc, press ){
  #----------------------------------------------------------------   
  # Calculates density of water at a given temperature and pressure
  # Ref: Chen et al. (1977)
  #----------------------------------------------------------------   
  # # arguments
  # real, intent(in) :: tc     # air temperature (degrees C)
  # real, intent(in) :: press  # atmospheric pressure (Pa)

  # # local variables
  # real :: po, ko, ca, cb
  # real :: pbar               # atmospheric pressure (bar)

  # # function return value
  # real :: density_h2o  # density of water, kg/m^3

  # Calculate density at 1 atm:
  po <- 0.99983952 
	  + 6.788260e-5  *tc 
	  - 9.08659e-6   *tc*tc 
	  + 1.022130e-7  *tc*tc*tc   
	  - 1.35439e-9   *tc*tc*tc*tc 
	  + 1.471150e-11 *tc*tc*tc*tc*tc 
	  - 1.11663e-13  *tc*tc*tc*tc*tc*tc 
	  + 5.044070e-16 *tc*tc*tc*tc*tc*tc*tc 
	  - 1.00659e-18  *tc*tc*tc*tc*tc*tc*tc*tc

  # Calculate bulk modulus at 1 atm:
  ko <- 19652.17 
	  + 148.1830   *tc 
	  - 2.29995    *tc*tc 
	  + 0.01281    *tc*tc*tc 
	  - 4.91564e-5 *tc*tc*tc*tc 
	  + 1.035530e-7*tc*tc*tc*tc*tc

  # Calculate temperature dependent coefficients:
  ca <- 3.26138 
	  + 5.223e-4  *tc 
	  + 1.324e-4  *tc*tc 
	  - 7.655e-7  *tc*tc*tc 
	  + 8.584e-10 *tc*tc*tc*tc

  cb <- 7.2061e-5 
	  - 5.8948e-6  *tc 
	  + 8.69900e-8 *tc*tc 
	  - 1.0100e-9  *tc*tc*tc 
	  + 4.3220e-12 *tc*tc*tc*tc

  # Convert atmospheric pressure to bar (1 bar <- 100000 Pa)
  pbar <- (1.0e-5)*press

  density_h2o <- 1000.0*po*(ko + ca*pbar + cb*pbar^2.0)/(ko + ca*pbar + cb*pbar^2.0 - pbar)

  return(density_h2o)

}


calc_psychro <- function( tc, press, par_splash ){
  #----------------------------------------------------------------   
  # Calculates the psychrometric constant for a given temperature and pressure
  # Ref: Allen et al. (1998); Tsilingiris (2008) 
  #----------------------------------------------------------------   
  # # arguments
  # real, intent(in) :: tc     # air temperature, degrees C
  # real, intent(in) :: press  # atmospheric pressure, Pa

  # # local variables
  # real :: lv  # latent heat of vaporization (J/kg)
  # real :: cp

  # # function return value
  # real :: psychro  # psychrometric constant, Pa/K

  # # local variables
  # real :: my_tc    # adjusted temperature to avoid numerical blow-up 

  # Adopted temperature adjustment from SPLASH, Python version
  my_tc <- tc

	my_tc <- min(max(tc, 0), 100)

  # Calculate the specific heat capacity of water, J/kg/K
  # Eq. 47, Tsilingiris (2008)
  cp <- 1.0e3*(1.0045714270 
			    + 2.050632750e-3  *my_tc 
				  - 1.631537093e-4  *my_tc*my_tc 
				  + 6.212300300e-6  *my_tc*my_tc*my_tc 
				  - 8.830478888e-8  *my_tc*my_tc*my_tc*my_tc 
				  + 5.071307038e-10 *my_tc*my_tc*my_tc*my_tc*my_tc
          )

  # Calculate latent heat of vaporization, J/kg
  lv <- calc_enthalpy_vap(tc)

  # Calculate psychrometric constant, Pa/K
  # Eq. 8, Allen et al. (1998)
  psychro <- cp * par_splash$kMa * press / (par_splash$kMv * lv)

  return(psychro)

}


