calc_soilparams <- function(df, method = "saxtonrawls"){
  
  if (method == "saxtonrawls"){
    
    # ADOPTED FROM soil_hydro.R by David Calle Sandoval
    # Soil Hydrophysics
    # ************************************************************************
    # Name:     soil_hydro
    # Input:    - float, fsand, (percent)
    #           - float, fclay, (percent)
    #           - float, forg Organic Matter (percent)
    #           - float,fgravel, (percent-volumetric)
    # Output:   list:
    #           - float, FC, (volumetric fraction)
    #           - float, PWP (volumetric fraction)
    #           - float, SAT, (volumetric fraction)
    #           - float, AWC (volumetric fraction)
    #           - float,Ksat, Saturate hydraulic conductivity/infiltration capacity(mm/hr)
    #           - float, A (Coefficient)
    #           - float, B (Clapp and Hornberger (1978) pore-size distribution index)
    # Features: calculate some soil hydrophysic characteristics
    # Ref:      Saxton, K.E., Rawls, W.J., 2006. Soil Water Characteristic Estimates 
    #           by Texture and Organic Matter for Hydrologic Solutions. 
    #           Soil Sci. Soc. Am. J. 70, 1569. doi:10.2136/sssaj2005.0117
    # ************************************************************************
    df <- df %>%
      
      ## use fractions for calculations
      mutate( fsand = fsand / 100, 
              fclay = fclay / 100,
              forg = forg / 100,
              fgravel = fgravel / 100 ) %>% 
      
      ## auxiliary variables
      rowwise() %>%
      mutate( fsand_forg = fsand * forg,
              fclay_forg = fclay * forg,
              fsand_fclay = fsand * fclay) %>%
      
      ## calculate field capacity FC
      mutate( fc0 = -0.251*fsand + 0.195*fclay + 0.011*forg + 0.006*(fsand_forg)-0.027*(fclay_forg) + 0.452*(fsand_fclay) + 0.299 ) %>%
      mutate( fc = fc0 + (1.283 * fc0^2 - 0.374 * fc0 - 0.015)) %>%
      select( -fc0 ) %>%
      
      ## calculate permanent wilting point PWP
      mutate( pwp0 = -0.024*fsand + 0.487*fclay + 0.006*forg + 0.005*(fsand_forg)-0.013*(fclay_forg) + 0.068*(fsand_fclay) + 0.031) %>%
      mutate( pwp = pwp0 + (0.14 * pwp0 - 0.02) ) %>%
      select( -pwp0 ) %>%
      
      ## calculate actual water holding capacity WHC
      mutate( whc = (fc - pwp) * (1.0 - fgravel/100) ) %>%
      
      ## calculate saturation water content
      mutate( sat0 = 0.278*fsand + 0.034*fclay + 0.022*forg-0.018*(fsand_forg)-0.027*(fclay_forg)-0.584*(fsand_fclay) + 0.078 ) %>%
      mutate( sat1 =  sat0 + (0.636*sat0-0.107) ) %>%
      mutate( sat = fc + sat1 - 0.097*fsand + 0.043 ) %>%
      select( -sat0, -sat1 ) %>%
      
      ## calculate conductivity at saturation KSAT
      mutate(coef_B = (log(1500)-log(33))/(log(fc)-log(pwp))) %>%
      mutate(coef_A = exp(log(33)+coef_B*log(fc))) %>%
      mutate(coef_lambda = 1/coef_B) %>%
      mutate(ksat = 1930*(sat-fc)^(3-coef_lambda)) %>%
      select(-coef_A, -coef_B, -coef_lambda) 
  
  } else if (method == "ballard"){
    
    ## ADOPTED FROM Hydrophysics_V2.R by David Calle Sandoval
    # Hydrophysics V2
    # ************************************************************************
    # Name:     soil_hydro
    # Input:    - float, fsand, (percent)
    #           - float, fclay, (percent)
    #           - float, OM Organic Matter (percent)
    #           - float,fgravel, (percent-volumetric)
    # Output:   list:
    #           - float, FC, (volumetric fraction)
    #           - float, WP (volumetric fraction)
    #           - float,SAT, (volumetric fraction)
    #           - float, AWC (volumetric fraction)
    #           - float,Ksat, Saturate hydraulic conductivity/infiltration capacity(mm/hr)
    #           - float, A (Coefficient)
    #           - float, B (Clapp and Hornberger (1978) pore-size distribution index)
    # Features: calculate some soil hydrophysic characteristics
    # Ref:      Saxton, K.E., Rawls, W.J., 2006. Soil Water Characteristic Estimates 
    #           by Texture and Organic Matter for Hydrologic Solutions. 
    #           Soil Sci. Soc. Am. J. 70, 1569. doi:10.2136/sssaj2005.0117
    # ************************************************************************
      
    depth <- 30
    topsoil <- 1
    
    ## needed
    if (!is.element("bd", names(df))) df <- df %>% mutate(bd = NA)

    df <- df %>% 
      
      ## use fractions for calculations
      mutate( fsand = fsand / 100, 
              fclay = fclay / 100,
              forg = forg / 100,
              fgravel = fgravel / 100 ) %>% 

      ## auxiliary variables
      rowwise() %>%
      mutate( fsand_forg = fsand * forg,
              fclay_forg = fclay * forg,
              fsand_fclay = fsand * fclay) %>%
      
      ## particle density?
      mutate( dp = 1/((forg/1.3)+((1-forg)/2.65)) ) %>% 
      
      ## buld density?
      mutate( bd = ifelse(is.na(bd), (1.5 + (dp-1.5-1.10*(1 - fclay))*(1-exp(-0.022*depth)))/(1+6.27*forg), bd ) ) %>% 
      
      ## water storage at saturation?
      mutate( sat = 1-(bd/dp) ) %>% 
    
      ## field capacity
      mutate( fc = (sat/bd)*(0.3366685 + (1.417544 - 0.3366685)*fclay^0.5)*exp(-(0.03320495*fsand - 0.2755312* forg)/(sat/bd)) ) %>% 
      mutate( fc = ifelse(fc<0, -0.1, fc) ) %>%  ## shouldn't it be zero?
      mutate( fc = ifelse(fc>1, 1, fc)) %>% 
      
      ## wilting point
      mutate( pwp = fc*(0.1437904 + (0.8398534 - 0.1437904)*fclay^0.5) ) %>% 
      
      ## conductivity at saturation
      mutate( L_10_Ksat = -2.793574+3.12048*log10(dp-bd)+4.358185*fsand ) %>% 
      mutate( ksat = 10^L_10_Ksat) %>% 
      mutate( ksat = ksat * 10 ) %>%  ## to mm/h
      select( - L_10_Ksat ) %>% 
      
      ## 
      mutate( moist_fvol33init = 0.278*fsand+0.034*fclay+0.022*forg-0.018*(fsand_forg)-0.027*(fclay_forg)-0.584*(fsand_fclay)+0.078) %>% 
      mutate( moist_fvol33 = moist_fvol33init+(0.636*moist_fvol33init-0.107) ) %>% 
            
      # get parameters for BC eqn form SAxton 2006
      mutate( coef_B = (log(1500)-log(33))/(log(fc)-log(pwp)) ) %>% 
      mutate( coef_A = exp(log(33)+coef_B*log(fc)) ) %>% 
      mutate( coef_lambda = 1/coef_B ) %>% 
      
      # Ksat<-1930*(SAT_fvol-FC_fvol)^(3-coef_lambda)
      
      mutate( bub_init = -21.6*fsand-27.93*fclay-81.97*moist_fvol33+71.12*(fsand*moist_fvol33)+8.29*(fclay*moist_fvol33)+14.05*(fsand_fclay)+27.16 ) %>% 
      mutate( bubbling_p = bub_init+(0.02*bub_init^2-0.113*bub_init-0.7) ) %>% 
      
      # 101.97162129779 converts from KPa to mmH2O
      mutate( bubbling_p = bubbling_p*-101.97162129779 ) %>% 
      mutate( bubbling_p = ifelse(bubbling_p>0, bubbling_p*-1, bubbling_p) ) %>%  # error in empirical fitting, not possible matric potential positive
      
      # residual water content for BC eqn, Rawls, 1985
      mutate( fsand = fsand * 100, 
              fclay = fclay * 100,
              silt = 100 - fsand - fclay,
              forg = forg * 100) %>% 

      # Ksat<-10*2.54*10^(-0.6+0.012*fsand-0.0064*fclay)
      mutate( RES = -0.018+0.0009*fsand+0.005*fclay+0.029*sat -0.0002*fclay^2-0.001*fsand*sat-0.0002*fclay^2*sat^2+0.0003*fclay^2*sat -0.002*sat^2*fclay ) %>% 

      # parameters for van Genutchen eqn
      mutate( VG_alpha = exp(-14.96 + 0.03135*fclay + 0.0351*silt + 0.646*forg +15.29*dp - 0.192*topsoil -4.671*dp^2- 0.000781*fclay^2 - 0.00687*forg^2 + 0.0449/forg + 0.0663*log(silt) + 0.1482*log(forg) - 0.04546*dp *silt - 0.4852*dp*forg + 0.00673*topsoil*fclay) ) %>% 
      mutate( VG_n = 1.0+exp(-25.23 - 0.02195*fclay + 0.0074*silt - 0.1940*forg + 45.5*dp - 7.24*dp^2 +0.0003658*fclay^2 + 0.002885*forg^2 -12.81/dp - 0.1524/silt - 0.01958/forg - 0.2876*log(silt) - 0.0709*log(forg) -44.6*log(dp) - 0.02264*dp*fclay + 0.0896*dp*forg +0.00718*topsoil*fclay) ) %>% 
      mutate( VG_m = 1-(VG_n) ) %>% 
      
      ## water holding capacity
      mutate( whc = (fc-pwp)*(1-fgravel) )

  }
  return(df) 
} 


