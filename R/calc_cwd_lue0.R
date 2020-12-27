calc_cwd_lue0 <- function(df, inst, nam_lue, do_plot = FALSE){  

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
    
    ## get linear fit with outliers
    linmod <- try(lm(lue ~ deficit, data = df))
    if (class(linmod) != "try-error"){

      ## remove outliers (residuals beyond 1.5 * IQR)
      idx_drop <- which(is.na(remove_outliers(linmod$residuals, coef = 1.5)))
      df$lue[idx_drop] <- NA
      
      ## git linear fit without outliers
      linmod <- try(lm(lue ~ deficit, data = df))
      
      if (class(linmod) != "try-error"){

        ## test: is slope negative?
        slope_lue <- coef(linmod)["deficit"]
        is_neg <- slope_lue < 0.0
        rsq <- summary(linmod)$r.squared
        
        if (is_neg){
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

      } else {
        is_sign <- FALSE
        slope_lue <- NA
        rsq <- NA
      }
      
    } else {
      is_sign <- FALSE
      cwd_lue0 <- NA
      slope_lue <- NA
      rsq <- NA
    }
    
    ##-----------------------------------------
    ## get exponential fit over time (Teuling et al. 2006 GRL)
    ##-----------------------------------------
    df_log <- df %>% 
      dplyr::filter(!is.na(lue)) %>% 
      mutate(log_lue = log(lue)) %>% 
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
    
    # expmod <- try( nls(lue ~ a * exp(k * deficit), data = df, start = list(a = 0.1, k = 1/50)) )
    # if (class(expmod) != "try-error"){
    # 
    #   df_coef <- summary(expmod) %>% coef()
    # 
    #   ## test: is slope negative?
    #   k_lue <- df_coef["k", "Estimate"]
    #   is_neg_exp <- k_lue < 0
    # 
    #   if (is_neg_exp){
    #     ## test: is slope significantly (5% level) different from zero (t-test)?
    #     is_sign_exp <- df_coef["k", "Pr(>|t|)"] < 0.05
    # 
    #     if (is_sign_exp){
    # 
    #       ## get CWD at wich lue is at 1/2e
    #       cwd_lue0_exp <- -1.0/coef(expmod)["k"]   ## CWD where lue is reduced to 1/e
    #       df_fit_exp <- tibble(y = predict(expmod, newdata = df), x = df$deficit)
    # 
    #     } else {
    #       cwd_lue0_exp <- NA
    #     }
    #   } else {
    #     is_sign_exp <- FALSE
    #     cwd_lue0_exp <- NA
    #     k_lue <- NA
    #   }
    # 
    # } else {
    #   is_sign_exp <- FALSE
    #   cwd_lue0_exp <- NA
    #   k_lue <- NA
    # }

    if (do_plot){
      gg <- df %>%
        mutate(iinst = as.factor(iinst)) %>%
        ggplot() +
        geom_point(aes(x = deficit, y = lue, color = iinst), alpha = 0.5) +
        labs( x = "Cumulative water deficit (mm)", y = nam_lue, subtitle = plot_title) +
        geom_hline(yintercept = 0.0, linetype = "dotted")
      # ylim(-0.1, 0.5)
      
      if (is_sign){
        gg <- gg +
          geom_vline(xintercept = cwd_lue0, linetype = "dotted", color = 'tomato') +
          geom_line(data = df_fit, aes(x, y), col = "tomato")
      }
      # if (is_sign_exp){
      #   gg <- gg +
      #     geom_vline(xintercept = cwd_lue0_exp, linetype = "dotted", color = 'royalblue') +
      #     geom_line(data = df_fit_exp, aes(x, y), col = "royalblue")
      # }
      
      # if (!identical(gumbi_alexi$mod, NA)){
      #   mctXX_alexi <- gumbi_alexi$df_return %>% dplyr::filter(return_period == use_return_period) %>% pull(return_level)
      #   if (!is.na(mctXX_alexi)){
      #     gg <- gg +
      #       geom_vline(xintercept = mctXX_alexi)
      #   } else {
      #     rlang::warn(paste("No ALEXI MCT outputs for site"))
      #   }
      # } else {
      #   rlang::warn(paste("No ALEXI MCT outputs for site"))
      #   mctXX_alexi <- NA
      # }
      
    } else {
      gg <- NULL
    }
    
    ## maximum cwd in observation period
    cwdmax <- max(df$deficit, na.rm = TRUE)
    
    ## Get "fLUE" (reduction in lue by bin)
    nbin <- 5
    df <- df %>%
      ungroup() %>% 
      mutate(bin = cut(deficit, breaks = nbin))
    df_agg <- df %>%
      ungroup() %>%
      group_by(bin) %>%
      summarise(lue = median(lue, na.rm = TRUE), .groups = "drop")

    ## get fractional reduction in LUE in highest CWD bin, relative to first bin
    flue <- (df_agg %>% slice(nbin) %>% pull(lue)) / (df_agg %>% slice(1) %>% pull(lue))
    flue <- ifelse(length(flue)==0, NA, flue)
    
    ## Get shape of lue reduction as a function of normalized CWD (normalized to observed maximum)
    df_flue <- df %>% 
      ungroup() %>% 
      mutate(deficit_norm = deficit / cwdmax) %>% 
      mutate(bin = cut(deficit_norm, breaks = nbin)) %>%
      group_by(bin) %>%
      summarise(lue = median(lue, na.rm = TRUE), .groups = "drop") %>% 
      mutate(flue = lue / .$lue[1]) %>% 
      dplyr::select(bin, flue)
    
  } else {

    # rlang::inform(paste0("Not enough lue data points for site ", sitename))
    cwd_lue0 <- NA
    slope_lue <- NA
    # cwd_lue0_exp <- NA
    # k_lue <- NA
    gg <- NA
    flue <- NA
    cwdmax <- NA
    rsq <- NA
    df_flue <- tibble(bin = NA, flue = NA)
    flue <- NA
    df_log <- tibble(intercept = NA, k_decay = NA, s0_teuling = NA)
    
  }

  return(list(cwd_lue0 = cwd_lue0, 
              slope_lue = slope_lue, 
              rsq = rsq, 
              # cwd_lue0_exp = cwd_lue0_exp, 
              # k_lue = k_lue, 
              gg = gg, 
              flue = flue, 
              cwdmax = cwdmax,
              lue_cwd0 = df_log$intercept[1],
              lambda_decay = 1.0 / df_log$k_decay[1],
              s0_teuling = df_log$s0_teuling[1],
              df_flue = df_flue
              ))

}
