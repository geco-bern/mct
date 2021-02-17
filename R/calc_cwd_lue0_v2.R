calc_cwd_lue0 <- function(df, inst, nam_lue, do_plot = FALSE, verbose = FALSE){  

  if (nam_lue=="SIF"){
    plot_title <- "ET: ALEXI, precipitation: WATCH-WFDEI, SIF: Duveiller et al."
  } else if (nam_lue=="fet"){
    plot_title <- "ET: ALEXI, precipitation: WATCH-WFDEI, net radiation: GLASS"
  } else {
    plot_title <- ""
  }
  
  ## rename
  icol <- which(nam_lue == names(df))
  newnames <- names(df) 
  newnames[icol] <- "lue"
  names(df) <- newnames
  
  ## doesn't always work
  # df <- df %>% 
  #   rename(lue = !!nam_lue)
  
  ## retain only data from largest instance of each year
  biginstances <- inst %>% 
    mutate(year = lubridate::year(date_start)) %>% 
    group_by(year) %>% 
    dplyr::filter(deficit == max(deficit)) %>% 
    pull(iinst)

  df <- df %>% 
    dplyr::filter(iinst %in% biginstances)
  
  nlue <- sum(!is.na(df$lue))


  if (nlue > 10){

    ## Bin data along deficit and make regression with upper 90% quantile
    nbin <- 50
    df <- df %>%
      ungroup() %>% 
      mutate(bin = cut(deficit, breaks = nbin)) %>% 
      mutate(cwd_lower = as.numeric( sub("\\((.+),.*", "\\1", bin)),
             cwd_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", bin) )
             ) %>% 
      mutate(cwd_mid = (cwd_lower + cwd_upper)/2) %>% 
      dplyr::select(-bin, -cwd_lower, -cwd_upper)

    df_agg <- df %>%
      ungroup() %>%
      group_by(cwd_mid) %>%
      summarise(lue = quantile(lue, probs = 0.9, na.rm = TRUE)) %>% 
      as.data.frame()

    ## Get "fLUE" (reduction in lue by bin)
    flue <- (df_agg %>% slice(nbin) %>% pull(lue)) / (df_agg %>% slice(1) %>% pull(lue))
    flue <- ifelse(length(flue)==0, NA, flue)      

    ## fit simple linear regression
    linmod <- lm(lue ~ cwd_mid, data = df_agg)

    ## get x-axis cutoff (plain)
    cwd_lue0 <- - coef(linmod)["(Intercept)"] / coef(linmod)["cwd_mid"]

    ## select the number of breakpoints based on BIC (own implementation here)
    try_bic <- function(mod){
      if (identical(class(mod), "try-error")){
        out <- 9999
      } else {
        out <- BIC(mod)
      }
      return(out)
    }
    list_lm_selg <- purrr::map(as.list(c(1,2)),
                              ~try(segmented(linmod, seg.Z = ~ cwd_mid, npsi=., silent=TRUE)))
    for (i in 2:1){
      list_lm_selg[[i+1]] <- list_lm_selg[[i]]
    }
    list_lm_selg[[1]] <- linmod
    vec_bic <- purrr::map_dbl(list_lm_selg, ~try_bic(.))
    idx_best <- which.min(vec_bic)
    npsi <- idx_best - 1
    lm_selg <- list_lm_selg[[idx_best]]

    has_cp <- FALSE

    if (!(class(lm_selg) == "try-error")){
      
      if (npsi > 0){
        ##--------------------------------------------------
        ## has changepoint      
        ##--------------------------------------------------
        if (verbose) rlang::inform("Change point detected.")
        has_cp <- TRUE
        
        ## metric
        rsq <- summary(lm_selg)$adj.r.squared
        
        ## add predictions
        df_agg <- df_agg %>% 
          mutate(.fit = predict(lm_selg, newdata = .))
        
        ## get first changepoint
        cp1 <- lm_selg$psi %>% as_tibble() %>% slice(1) %>% pull(Est.)
        
        ## get useful coefficients
        b0 <- coef(lm_selg)[["(Intercept)"]]
        slope1 <- coef(lm_selg)[["cwd_mid"]]
        
        ## Important: the coefficients are the differences in slope in comparison to the previous slope
        slope2 <- coef(lm_selg)[["cwd_mid"]] + coef(lm_selg)[["U1.cwd_mid"]]
        
        ## Solve for c0 (intercept of second segment):
        c0 <- b0 + slope1 * cp1 - slope2 * cp1
        
        # ## is slope 1 negative?
        # is_neg1 <- slope1 < 0
        
        # ## is slope 2 negative?
        # is_neg2 <- slope2 < 0
        
        # ## is slope 1 significant?
        # is_sign <- coef(summary(lm_selg))["cwd_mid", "Pr(>|t|)"] < 0.05
        
        if (slope1 < slope2){
          ## flattening after first CP
          
          if (npsi == 1){
            ##--------------------------------------------------
            ## (A) single changepoint 
            ##--------------------------------------------------
            
            if (slope1 < 0){
              ##--------------------------------------------------
              ## (Aa) single changepoint  flattening
              ##--------------------------------------------------
              cwd_lue0 <- NA
              
              ## slope 2 less negative than slope 1 and significantly different?
              df_coef <- coef(summary(lm_selg)) %>% as_tibble(rownames = "coef")
              is_flattening <- (slope1 < 0) && (slope1 < slope2) && (df_coef %>% dplyr::filter(coef == "U1.cwd_mid") %>% pull("Pr(>|t|)") < 0.05)        
              
              type <- "Aa"
              
            } else {
              ##--------------------------------------------------
              ## (Ab) single changepoint stronger increase - WEIRD
              ##--------------------------------------------------
              cwd_lue0 <- NA
              is_flattening <- NA
              type <- "Ab"
              
            }
            
            
          } else if (npsi == 2){
            ##--------------------------------------------------
            ## Two changepoints
            ##--------------------------------------------------
            slope3 <- coef(lm_selg)[["U2.cwd_mid"]] + slope2
            cp2 <- lm_selg$psi %>% as_tibble() %>% slice(2) %>% pull(Est.)
            
            ## Solve for d0 (intercept of third segment):
            d0 <- c0 + slope2 * cp2 - slope3 * cp2
            
            
            if (slope2 < slope3){
              ##--------------------------------------------------
              ## (A1) flattening at CP1
              ##--------------------------------------------------
              cwd_lue0 <- NA
              
              ## slope 2 less negative than slope 1 and significantly different?
              df_coef <- coef(summary(lm_selg)) %>% as_tibble(rownames = "coef")
              is_flattening <- (slope1 < 0) && (slope1 < slope2) && (df_coef %>% dplyr::filter(coef == "U1.cwd_mid") %>% pull("Pr(>|t|)") < 0.05)        
              type <- "A1"
              
            } else {
              
              ## flattening at cp1?
              
              if (slope3 < 0){
                ##--------------------------------------------------
                ## (A2) declining after CP2
                ##--------------------------------------------------
                ## x-axis cutoff based on third slope
                cwd_lue0 <- - d0 / slope3
                
                if ((cwd_lue0 > 2*max(df$deficit))){
                  ## too much extrapolation - interpret as flattening at CP1
                  cwd_lue0 <- NA
                  df_coef <- coef(summary(lm_selg)) %>% as_tibble(rownames = "coef")
                  is_flattening <- (slope1 < 0) && (slope1 < slope2) && (df_coef %>% dplyr::filter(coef == "U1.cwd_mid") %>% pull("Pr(>|t|)") < 0.05)   
                  type <- "A2a"
                  
                } else {
                  ## actually declining after CP2 - take slope 3 for x-axis cutoff
                  df_coef <- coef(summary(lm_selg)) %>% as_tibble(rownames = "coef")
                  is_flattening <- (slope1 < 0) && (slope1 < slope2) && (df_coef %>% dplyr::filter(coef == "U1.cwd_mid") %>% pull("Pr(>|t|)") < 0.05)   
                  type <- "A2b"
                }
                
              } else {
                ##--------------------------------------------------
                ## (A2a) similar as above: slope3 is positive - flattening at cp1 and no x-axis intercept
                ##--------------------------------------------------
                cwd_lue0 <- NA
                df_coef <- coef(summary(lm_selg)) %>% as_tibble(rownames = "coef")
                is_flattening <- (slope1 < 0) && (slope1 < slope2) && (df_coef %>% dplyr::filter(coef == "U1.cwd_mid") %>% pull("Pr(>|t|)") < 0.05)   
                type <- "A2a"              
              }
              
            }
            
          } else {
            ## more than 2 breakpoints - not considered
            cwd_lue0 <- NA
            is_flattening <- NA
            type <- NA
            
          }
          
          
        } else {
          
          if (npsi == 1){
            ##--------------------------------------------------
            ## (B) single changepoint
            ##--------------------------------------------------
            if (slope2 > 0){
              ##--------------------------------------------------
              ## (Ba) plateau
              ##--------------------------------------------------
              cwd_lue0 <- NA
              is_flattening <- NA
              type <- "Ba"
              
            } else {
              ##--------------------------------------------------
              ## (Bb) decline after initial plateau (slope 2 is negative)
              ##--------------------------------------------------
              ## take slope 2 for x-axis cutoff
              cwd_lue0 <- - c0 / slope2
              is_flattening <- FALSE
              type <- "Bb"
              
            }
            
            
          } else if (npsi == 2){
            ##--------------------------------------------------
            ## Two changepoints
            ##--------------------------------------------------
            cp2 <- lm_selg$psi %>% as_tibble() %>% slice(2) %>% pull(Est.)
            slope3 <- coef(lm_selg)[["U2.cwd_mid"]] + slope2
            
            ## Solve for d0 (intercept of third segment):
            d0 <- c0 + slope2 * cp2 - slope3 * cp2
            
            
            if (slope2 < slope3){
              ##--------------------------------------------------
              ## (B1) increasing weirdly at the end - WEIRD
              ##--------------------------------------------------
              cwd_lue0 <- NA
              is_flattening <- NA
              type <- "B1"
              
            } else {
              
              if (slope3 < 0){
                ##--------------------------------------------------
                ## (B2) declining after intermediate plateau
                ##--------------------------------------------------
                ## take slope 3 for x-axis cutoff
                cwd_lue0 <- - d0 / slope3
                is_flattening <- FALSE
                type <- "B2"
                
              } else {
                ##--------------------------------------------------
                ## (B1a) Flattening at both CP by still positive slope - WEIRD
                ##--------------------------------------------------
                cwd_lue0 <- NA
                is_flattening <- NA
                type <- "B1a"              
              }
              
            }
            
          } else {
            ## more than 2 breakpoints - not considered
            cwd_lue0 <- NA
            is_flattening <- NA
            type <- NA
            
          }
          
        }
        
      }

    }

    if (!has_cp) {
      ##--------------------------------------------------
      ## has no changepoint      
      ##--------------------------------------------------
      if (verbose) rlang::inform("No change point detected.")
      has_cp <- FALSE
      cp1 <- NA
      type <- "0"


      ## metric
      rsq <- summary(linmod)$adj.r.squared

      ## add predictions (simple linear regression model)
      df_agg <- df_agg %>% 
        mutate(.fit = predict(linmod, newdata = .))
      
      ## slope of single linear regression
      slope1 <- coef(linmod)["deficit"]
      is_neg <- slope1 < 0.0

      if (is_neg && !is.na(is_neg)){
        ## test: is slope significantly (5% level) different from zero (t-test)?
        is_sign <- coef(summary(linmod))["deficit", "Pr(>|t|)"] < 0.05

        if (is_sign){
          ## get x-axis cutoff
          cwd_lue0 <- - coef(linmod)["(Intercept)"] / coef(linmod)["deficit"]
          df_fit <- tibble(y = predict(linmod, newdata = df), x = df$deficit)
        } else {
          cwd_lue0 <- NA
        }

      } else {
        is_sign <- FALSE
        cwd_lue0 <- NA
      }

      ## variables defined only if is segmeted regression
      slope2 <- NA
      cwd_lue0 <- NA
      is_flattening <- NA

    }
      
    ##-----------------------------------------
    ## get exponential fit over time (Teuling et al. 2006 GRL)
    ##-----------------------------------------
    df_log <- df %>% 
      dplyr::filter(!is.na(lue)) %>% 
      mutate(log_lue = log(lue)) %>% 
      mutate(log_lue = ifelse(is.nan(log_lue), NA, log_lue)) %>% 
      mutate(log_lue = ifelse(is.infinite(log_lue), NA, log_lue)) %>% 
      tidyr::drop_na(log_lue) %>% 
      group_by(iinst) %>% 
      nest() %>% 
      mutate(linmod = purrr::map(data, ~lm(log_lue ~ dday, data = .))) %>% 
      mutate(linmod = purrr::map(linmod, ~coef(.))) %>% 
      mutate(intercept = purrr::map_dbl(linmod, "(Intercept)")) %>% 
      mutate(k_decay = purrr::map_dbl(linmod, "dday")) %>% 
      mutate(intercept = exp(intercept)) %>% 
      mutate(s0_teuling = - intercept / k_decay) %>% 
      ungroup() %>% 
      summarise_at(vars(intercept, k_decay, s0_teuling), median, na.rm = TRUE, .groups = "drop")

    lambda_decay <- 1.0 / df_log$k_decay[1]
    s0_teuling <- df_log$s0_teuling[1]
    lue_cwd0 <- df_log$intercept[1]
    
    ## maximum cwd in observation period
    cwdmax <- max(df$deficit, na.rm = TRUE)
    
    ## avoid cases where x-axis intersect determined from first or second slope is smaller than the maximum CWD during time series
    tmp <- df_agg %>% 
      drop_na() %>% 
      pull(cwd_mid) %>% 
      max()
    cwd_lue0 <- max(cwd_lue0, tmp)
    
    ## avoid cases where cwd_lue0 is beyond twice the range of observed CWD values
    cwd_lue0 <- ifelse(cwd_lue0 > 2*cwdmax, NA, cwd_lue0)

    ## determine point of flattening
    if (!identical(NA, is_flattening)){
      if (is_flattening){
        if (type %in% c("A1", "Aa", "A2a")){
          cwd_flattening <- cp1
        } else {
          cwd_flattening <- NA
        }
      } else {
        cwd_flattening <- NA
      }
    } else {
      cwd_flattening <- NA
    }

    if (do_plot){

      ## visualise segmented model
      gg <- ggplot() +
        geom_point(aes(deficit, lue), data = df) +
        geom_point(aes(cwd_mid, lue), data = df_agg, color = "red", size = 3) +
        geom_line(aes(cwd_mid, .fit), data = df_agg, color = "red") +
        geom_hline(aes(yintercept = 0), linetype = "dotted") +
        labs(x = "CWD (mm)", y = "X(t)", title = paste("Type", type))
      
      if (!is.na(cwd_lue0)){
        gg <- gg +
          geom_vline(xintercept = cwd_lue0, color = 'red')
      }

      if (!is.na(cwd_flattening)){
        gg <- gg +
          geom_vline(aes(xintercept = cwd_flattening), color = "royalblue")
      }

    } else {
      gg <- NULL
    }
    
  } else {

    # rlang::inform(paste0("Not enough lue data points for site ", sitename))
    type <- NA
    cwd_lue0 <- NA
    is_flattening <- NA
    gg <- NA
    flue <- NA
    cwdmax <- NA
    lambda_decay <- NA
    s0_teuling <- NA
    lue_cwd0 <- NA
    cwd_flattening <- NA
    
  }

  return(list(cwd_lue0      = cwd_lue0, 
              is_flattening = is_flattening,
              gg            = gg, 
              flue          = flue, 
              cwdmax        = cwdmax,
              lue_cwd0      = lue_cwd0,
              lambda_decay  = lambda_decay,
              s0_teuling    = s0_teuling,
              type          = type,
              cwd_flattening= cwd_flattening
              ))

}
