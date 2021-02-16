calc_cwd_lue0 <- function(df, inst, nam_lue, do_plot = FALSE, verbose = FALSE){  

  if (nam_lue=="SIF"){
    plot_title <- "ET: ALEXI, precipitation: WATCH-WFDEI, SIF: Duveiller et al."
  } else if (nam_lue=="fet"){
    plot_title <- "ET: ALEXI, precipitation: WATCH-WFDEI, net radiation: GLASS"
  } else {
    plot_title <- ""
  }
  
  ## rename
  df <- df %>% 
    rename(lue = !!nam_lue)
  
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
      summarise(lue = quantile(lue, probs = 0.9, na.rm = TRUE), .groups = "drop")  

    ## Get "fLUE" (reduction in lue by bin)
    flue <- (df_agg %>% slice(nbin) %>% pull(lue)) / (df_agg %>% slice(1) %>% pull(lue))
    flue <- ifelse(length(flue)==0, NA, flue)      

    ## fit simple linear regression
    linmod <- try(lm(lue ~ cwd_mid, data = df_agg))

    ## get x-axis cutoff (plain)
    cwd_lue0 <- - coef(linmod)["(Intercept)"] / coef(linmod)["cwd_mid"]

    ## select the number of breakpoints
    lm_selg <- selgmented(linmod, seg.Z = ~cwd_mid, return.fit = FALSE)

    if (lm_selg$npsi > 0){
      ##--------------------------------------------------
      ## has changepoint      
      ##--------------------------------------------------
      if (verbose) rlang::inform("Change point detected.")
      has_cp <- TRUE
      
      ## fit again, now returning fit
      lm_selg <- selgmented(linmod, seg.Z = ~cwd_mid, return.fit = TRUE)

      ## metric
      rsq <- summary(lm_selg)$adj.r.squared

      ## add predictions
      df_agg <- df_agg %>% 
        mutate(.fit = predict(lm_selg, newdata = .))

      ## save changepoint
      cp <- lm_seg$psi %>% as_tibble() %>% pull(Est.)

      ## get useful coefficients
      b0 <- coef(lm_selg)[[1]]
      slope_lue <- coef(lm_selg)[[2]]

      ## Important: the coefficients are the differences in slope in comparison to the previous slope
      slope2 <- coef(lm_selg)[[2]] + coef(lm_selg)[[3]]

      # ## Solve for c0 (intercept of second segment):
      # c0 <- b0 + slope_lue * cp - slope2 * cp

      ## is slope 1 negative?
      is_neg <- slope_lue < 0

      ## is slope 1 significant?
      is_sign <- coef(summary(lm_selg))["cwd_mid", "Pr(>|t|)"] < 0.05

      if (is_sign){

        ## cwd_lue0 based on first slope
        cwd_lue0 <- - b0 / slope_lue

        ## slope 2 less negative than slope 1 and significantly different?
        df_coef <- coef(summary(lm_selg)) %>% as_tibble(rownames = "coef")
        is_flattening <- (slope_lue < 0) && (slope_lue < slope2) && (df_coef %>% dplyr::filter(coef == "U1.cwd_mid") %>% pull("Pr(>|t|)") < 0.05)        
      
      } else {

        cwd_lue0 <- NA
        is_flattening <- NA

      }


    } else {
      ##--------------------------------------------------
      ## has no changepoint      
      ##--------------------------------------------------
      if (verbose) rlang::inform("No change point detected.")
      has_cp <- FALSE
      cp <- NA

      ## metric
      rsq <- summary(linmod)$adj.r.squared

      ## add predictions (simple linear regression model)
      df_agg <- df_agg %>% 
        mutate(.fit = predict(linmod, newdata = .))
      
      ## slope of single linear regression
      slope_lue <- coef(linmod)["deficit"]
      is_neg <- slope_lue < 0.0

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

    if (do_plot){

      ## visualise segmented model
      gg <- ggplot() +
        geom_point(aes(deficit, lue), data = df) +
        geom_point(aes(cwd_mid, lue), data = df_agg, color = "red", size = 3) +
        geom_line(aes(cwd_mid, .fit), data = df_agg, color = "red") +
        geom_hline(aes(yintercept = 0), linetype = "dotted") +
        geom_vline(aes(xintercept = cp), linetype = "dotted")
      
      if (is_sign){
        gg <- gg +
          geom_vline(xintercept = cwd_lue0, color = 'red')
      }
      
    } else {
      gg <- NULL
    }
    
    ## maximum cwd in observation period
    cwdmax <- max(df$deficit, na.rm = TRUE)
    
    
  } else {

    # rlang::inform(paste0("Not enough lue data points for site ", sitename))
    cwd_lue0 <- NA
    slope_lue <- NA
    has_cp <- NA
    cp <- NA
    slope2 <- NA
    is_flattening <- NA
    gg <- NA
    flue <- NA
    cwdmax <- NA
    lambda_decay <- NA
    s0_teuling <- NA
    lue_cwd0 <- NA
    
  }

  return(list(cwd_lue0      = cwd_lue0, 
              slope_lue     = slope_lue, 
              has_cp        = has_cp,
              cp            = cp,
              slope2        = slope2,
              is_flattening = is_flattening,
              gg            = gg, 
              flue          = flue, 
              cwdmax        = cwdmax,
              lue_cwd0      = lue_cwd0,
              lambda_decay  = lambda_decay,
              s0_teuling    = s0_teuling
              ))

}
