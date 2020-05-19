test_mct_bysite_alexi_fluxnet <- function(sitename, df_fluxnet, df_alexi, thresh_terminate = 0.2, thresh_drop = 0.8, use_return_period = 10, fittype = NULL){
  
  print(sitename)
  
  ## calculate daily water balance
  df_fluxnet <- df_fluxnet %>% 
    unnest(data) %>% 
    mutate(latenth_mm = ifelse(is.na(latenth_mm), 0, latenth_mm)) %>% 
    mutate(bal = prec - latenth_mm) %>% 
    mutate(bal = myapprox(bal))
  
  df_alexi <- df_alexi %>% 
    unnest(df) %>% 
    mutate(et_mm = ifelse(is.na(et_mm), 0, et_mm)) %>% 
    mutate(bal = prec - et_mm) %>% 
    mutate(bal = myapprox(bal)) %>% 
    drop_na(bal) %>% 
    ## merge FLUXNET GPP and PPFD data into the alexi data frame
    left_join(
      df_example_fluxnet %>% 
        ungroup() %>% 
        dplyr::select(date, gpp, ppfd),
      by = "date"
    )
  
  ## apply mct function------------------------------------------
  mct_fluxnet <- mct(df_fluxnet, varname_wbal = "bal", thresh_terminate = thresh_terminate, thresh_drop = thresh_drop )
  mct_alexi   <- mct(df_alexi,   varname_wbal = "bal", thresh_terminate = thresh_terminate, thresh_drop = thresh_drop )

  ## fit extreme value statistics  
  gumbi_alexi   <- get_plantwhc_mct_bysite(df_alexi,   varname_wbal = "bal", thresh_terminate = thresh_terminate, thresh_drop = thresh_drop, fittype = fittype)
  gumbi_fluxnet <- get_plantwhc_mct_bysite(df_fluxnet, varname_wbal = "bal", thresh_terminate = thresh_terminate, thresh_drop = thresh_drop, fittype = fittype)
  
  if (!dir.exists("fig/cwd_fluxnet")) system("mkdir -p fig/cwd_fluxnet")
  if (!dir.exists("fig/cwd_alexi")) system("mkdir -p fig/cwd_alexi")
  if (!dir.exists("fig/cwd_alexi_fluxnet")) system("mkdir -p fig/cwd_alexi_fluxnet")
  if (!dir.exists("fig/lue_cwd_fluxnet")) system("mkdir -p fig/lue_cwd_fluxnet")
  if (!dir.exists("fig/lue_cwd_alexi")) system("mkdir -p fig/lue_cwd_alexi")
  if (!dir.exists("fig/eor_cwd_fluxnet")) system("mkdir -p fig/eor_cwd_fluxnet")
  if (!dir.exists("fig/eor_cwd_alexi")) system("mkdir -p fig/eor_cwd_alexi")
  if (!dir.exists("fig/hist")) system("mkdir -p fig/hist")
  if (!dir.exists("fig/prec")) system("mkdir -p fig/prec")
  if (!dir.exists("fig/modobs_prec")) system("mkdir -p fig/modobs_prec")
  if (!dir.exists("fig/modobs_et")) system("mkdir -p fig/modobs_et")
  if (!dir.exists("fig/et")) system("mkdir -p fig/et")
  
  ## cumulative deficits------------------------------------------
  ## FLUXNET data
  ggplot() +
    geom_rect(
      data=mct_fluxnet$inst,
      aes(xmin=date_start, xmax=date_end, ymin=-99, ymax=99999),
      fill=rgb(0,0,0,0.3),
      color=NA) +
    geom_line(data = mct_fluxnet$df, aes(date, prec), size = 0.2, color="royalblue") +
    geom_line(data = mct_fluxnet$df, aes(date, deficit), color="tomato", size = 0.2) +
    geom_line(data = mct_fluxnet$df, aes(date, 100 * (gpp/ppfd)), size = 0.2) +
    coord_cartesian(ylim=c(0, max(mct_alexi$df$deficit, na.rm = TRUE))) +
    theme_classic() +
    labs(title = sitename, subtitle = "ET and precipitation: FLUXNET", x = "Date", y = "Cumulative water deficit (mm)")
  ggsave(paste0("fig/cwd_fluxnet/cwd_fluxnet_", sitename, ".pdf"), width = 6, height = 3)

  ## ALEXI data
  ggplot() +
    geom_rect(
      data=mct_alexi$inst,
      aes(xmin=date_start, xmax=date_end, ymin=-99, ymax=99999),
      fill=rgb(0,0,0,0.3),
      color=NA) +
    geom_line(data = mct_alexi$df, aes(date, prec), size = 0.2, color="royalblue") +
    geom_line(data = mct_alexi$df, aes(date, deficit), color="tomato", size = 0.2) +
    geom_line(data = mct_alexi$df, aes(date, 100 * (gpp/ppfd)), size = 0.2) +
    coord_cartesian(ylim=c(0, max(mct_alexi$df$deficit, na.rm = TRUE))) +
    theme_classic() +
    labs(title = sitename, subtitle = "ET: ALEXI, precipitation: WATCH-WFDEI", x = "Date", y = "Cumulative water deficit (mm)")
  ggsave(paste0("fig/cwd_alexi/cwd_alexi_", sitename, ".pdf"), width = 6, height = 3)
  

  ## ALEXI and FLUXNET data on top of each other
  # ggplot() +
  #   geom_line(data = mct_fluxnet$df, aes(date, deficit), color="black", size = 0.2) +
  #   geom_line(data = mct_alexi$df, aes(date, deficit), color="tomato", size = 0.2) +
  #   labs(title = sitename, subtitle = "ALEXI/WATCH vs FLUXNET", x = "Date", y = "Cumulative water deficit (mm)")
  
  ## ALEXI vs. FLUXNET: maximum CWD in each year
  df_max_inst <- mct_fluxnet$inst %>% 
    mutate(year = lubridate::year(date_start)) %>% 
    group_by(year) %>% 
    summarise(deficit_fluxnet = max(deficit)) %>% 
    full_join(
      mct_alexi$inst %>% 
        mutate(year = lubridate::year(date_start)) %>% 
        group_by(year) %>% 
        summarise(deficit_alexi = max(deficit)),
      by = "year"
    )
  n_avl <- df_max_inst %>% 
    mutate(avl =( !is.na(deficit_fluxnet) & !is.na(deficit_alexi))) %>% 
    summarise(avl = sum(avl)) %>% 
    pull(avl)
  if (n_avl > 2){
    out <- df_max_inst %>% 
      rbeni::analyse_modobs2("deficit_fluxnet", "deficit_alexi")
    out$gg
    ggsave(paste0("fig/cwd_alexi_fluxnet/cwd_alexi_fluxnet_", sitename, ".pdf"), width = 6, height = 3)
  }
  
  ## LUE vs CWD: FLUXNET------------------------------------------
    ## retain only data from largest instances of each year
    biginstances <- mct_fluxnet$inst %>% 
      mutate(year = lubridate::year(date_start)) %>% 
      group_by(year) %>% 
      dplyr::filter(deficit == max(deficit)) %>% 
      pull(iinst)
    
    mct_fluxnet$df <- mct_fluxnet$df %>% 
      dplyr::filter(iinst %in% biginstances) %>%
      mutate(lue = gpp/ppfd) 
    
    ## get linear fit with outliers
    linmod <- try(lm(lue ~ deficit, data = mct_fluxnet$df))
    if (class(linmod) != "try-error"){
      idx_drop <- which(is.na(remove_outliers(linmod$residuals, coef = 1.5)))
      mct_fluxnet$df$lue[idx_drop] <- NA
      
      ## git linear fit without outliers
      linmod <- try(lm(lue ~ deficit, data = mct_fluxnet$df))
      
      if (class(linmod) != "try-error"){
        
        ## test: is slope negative?
        is_neg <- coef(linmod)["deficit"] < 0.0
        
        if (is_neg){
          ## test: is slope significantly (5% level) different from zero (t-test)?
          is_sign <- coef(summary(linmod))["deficit", "Pr(>|t|)"] < 0.05
          if (is_sign){
            ## get x-axis cutoff
            lue0_fluxnet <- - coef(linmod)["(Intercept)"] / coef(linmod)["deficit"]
            df_fit <- tibble(y = predict(linmod, newdata = mct_fluxnet$df), x = mct_fluxnet$df$deficit)
          } else {
            lue0_fluxnet <- NA
          }
        } else {
          is_sign <- FALSE
          lue0_fluxnet <- NA
        }
      } else {
        is_sign <- FALSE
        lue0_fluxnet <- NA
      }
    } else {
      is_sign <- FALSE
      lue0_fluxnet <- NA
    }
    
    ## get exponential fit
    expmod <- try( nls(lue ~ a * exp(k * deficit), data = mct_fluxnet$df, start = list(a = 0.1, k = 1/50)) )
    if (class(expmod) != "try-error"){
      
      df_coef <- summary(expmod) %>% coef()
      
      ## test: is slope negative?
      is_neg_exp <- df_coef["k", "Estimate"] < 0

      if (is_neg_exp){
        ## test: is slope significantly (5% level) different from zero (t-test)?
        is_sign_exp <- df_coef["k", "Pr(>|t|)"] < 0.05
        
        if (is_sign_exp){
          
          ## get CWD at wich LUE is at 1/2e
          lue0_exp_fluxnet <- -1.0/coef(expmod)["k"]*2.0   ## CWD where LUE is reduced to 1/2e
          df_fit_exp <- tibble(y = predict(expmod, newdata = mct_fluxnet$df), x = mct_fluxnet$df$deficit)
          
        } else {
          lue0_exp_fluxnet <- NA
        }
      } else {
        is_sign_exp <- FALSE
        lue0_exp_fluxnet <- NA
      }
      
    } else {
      is_sign_exp <- FALSE
      lue0_exp_fluxnet <- NA
    }
    
    ## plot
    out <- mct_fluxnet$df %>% 
      ggplot(aes(x = deficit, y = lue)) +
      geom_point(alpha = 0.5) +
      labs(title = sitename, x = "Cumulative water deficit (mm)", y = "GPP/PPFD", subtitle = "ET: FLUXNET, precipitation: FLUXNET, GPP: FLUXNET, PPFD: FLUXNET") +
      ylim(-0.1, 0.5)

    if (is_sign){
      out <- out +
        geom_vline(xintercept = lue0_fluxnet, linetype = "dotted", color = 'tomato') +
        geom_line(data = df_fit, aes(x, y), col = "tomato")
    }  
    if (is_sign_exp){
      out <- out +
        geom_vline(xintercept = lue0_exp_fluxnet, linetype = "dotted", color = 'royalblue') +
        geom_line(data = df_fit_exp, aes(x, y), col = "royalblue")
    }
    if (!identical(gumbi_fluxnet$mod, NA)){
      mctXX_fluxnet <- gumbi_fluxnet$df_return %>% dplyr::filter(return_period == use_return_period) %>% pull(return_level)
      if (!is.na(mctXX_fluxnet)){
        out <- out +
          geom_vline(xintercept = mctXX_fluxnet)
      } else {
        rlang::warn(paste("No FLUXNET MCT outputs for site", sitename))
      }
    } else {
      rlang::warn(paste("No FLUXNET MCT outputs for site", sitename))
      mctXX_fluxnet <- NA
    }
    out
    ggsave(paste0("fig/lue_cwd_fluxnet/lue_cwd_fluxnet_", sitename, ".pdf"), width = 6, height = 4)

  
  ## LUE vs CWD: ALEXI/WATCH-WFDEI------------------------------------------
    ## retain only data from largest instances of each year
    biginstances <- mct_alexi$inst %>% 
      mutate(year = lubridate::year(date_start)) %>% 
      group_by(year) %>% 
      dplyr::filter(deficit == max(deficit)) %>% 
      pull(iinst)

    mct_alexi$df <- mct_alexi$df %>% 
      dplyr::filter(iinst %in% biginstances) %>%
      mutate(lue = gpp/ppfd) 
    
    ## get linear fit with outliers
    linmod <- try(lm(lue ~ deficit, data = mct_alexi$df))
    if (class(linmod) != "try-error"){
      idx_drop <- which(is.na(remove_outliers(linmod$residuals, coef = 1.5)))
      mct_alexi$df$lue[idx_drop] <- NA
      
      ## git linear fit without outliers
      linmod <- try(lm(lue ~ deficit, data = mct_alexi$df))
      
      if (class(linmod) != "try-error"){
        ## test: is slope negative?
        is_neg <- coef(linmod)["deficit"] < 0.0
        
        if (is_neg){
          ## test: is slope significantly (5% level) different from zero (t-test)?
          is_sign <- coef(summary(linmod))["deficit", "Pr(>|t|)"] < 0.05
          if (is_sign){
            ## get x-axis cutoff
            lue0_alexi <- - coef(linmod)["(Intercept)"] / coef(linmod)["deficit"]
            df_fit <- tibble(y = predict(linmod, newdata = mct_alexi$df), x = mct_alexi$df$deficit)
          } else {
            lue0_alexi <- NA
          }
        } else {
          is_sign <- FALSE
          lue0_alexi <- NA
        }
      } else {
        is_sign <- FALSE
        lue0_alexi <- NA
      }
    } else {
      is_sign <- FALSE
      lue0_alexi <- NA
    }
    
    ## get exponential fit
    expmod <- try( nls(lue ~ a * exp(k * deficit), data = mct_alexi$df, start = list(a = 0.1, k = 1/50)) )
    if (class(expmod) != "try-error"){
      
      df_coef <- summary(expmod) %>% coef()
      
      ## test: is slope negative?
      is_neg_exp <- df_coef["k", "Estimate"] < 0
      
      if (is_neg_exp){
        ## test: is slope significantly (5% level) different from zero (t-test)?
        is_sign_exp <- df_coef["k", "Pr(>|t|)"] < 0.05
        
        if (is_sign_exp){
          
          ## get CWD at wich LUE is at 1/2e
          lue0_exp_alexi <- -1.0/coef(expmod)["k"]*2.0   ## CWD where LUE is reduced to 1/2e
          df_fit_exp <- tibble(y = predict(expmod, newdata = mct_alexi$df), x = mct_alexi$df$deficit)
          
        } else {
          lue0_exp_alexi <- NA
        }
      } else {
        is_sign_exp <- FALSE
        lue0_exp_alexi <- NA
      }
      
    } else {
      is_sign_exp <- FALSE
      lue0_exp_alexi <- NA
    }
    
    out <- mct_alexi$df %>% 
      ggplot(aes(x = deficit, y = lue)) +
      geom_point(alpha = 0.5) +
      labs(title = sitename, x = "Cumulative water deficit (mm)", y = "GPP/PPFD", subtitle = "ET: ALEXI, precipitation: WATCH-WFDEI, GPP: FLUXNET, PPFD: FLUXNET") +
      ylim(-0.1, 0.5)
    
    if (is_sign){
      out <- out +
        geom_vline(xintercept = lue0_alexi, linetype = "dotted", color = 'tomato') +
        geom_line(data = df_fit, aes(x, y), col = "tomato")
    }
    if (is_sign_exp){
      out <- out +
        geom_vline(xintercept = lue0_exp_alexi, linetype = "dotted", color = 'royalblue') +
        geom_line(data = df_fit_exp, aes(x, y), col = "royalblue")
    }
    if (!identical(gumbi_alexi$mod, NA)){
      mctXX_alexi <- gumbi_alexi$df_return %>% dplyr::filter(return_period == use_return_period) %>% pull(return_level)
      if (!is.na(mctXX_alexi)){
        out <- out +
          geom_vline(xintercept = mctXX_alexi)
      } else {
        rlang::warn(paste("No ALEXI MCT outputs for site", sitename))
      }
    } else {
      rlang::warn(paste("No ALEXI MCT outputs for site", sitename))
      mctXX_alexi <- NA
    }
    out
    ggsave(paste0("fig/lue_cwd_alexi/lue_cwd_alexi_", sitename, ".pdf"), width = 6, height = 4)
    
  ## EOR vs CWD: FLUXNET----------------------
    ## retain only data from largest instances of each year
    biginstances <- mct_fluxnet$inst %>% 
      mutate(year = lubridate::year(date_start)) %>% 
      group_by(year) %>% 
      dplyr::filter(deficit == max(deficit)) %>% 
      pull(iinst)
    
    mct_fluxnet$df <- mct_fluxnet$df %>% 
      dplyr::filter(iinst %in% biginstances) %>%
      mutate(eor = latenth/netrad) 
    
    ## get linear fit with outliers
    linmod <- try(lm(eor ~ deficit, data = mct_fluxnet$df))
    if (class(linmod) != "try-error"){
      idx_drop <- which(is.na(remove_outliers(linmod$residuals, coef = 1.5)))
      mct_fluxnet$df$eor[idx_drop] <- NA
      
      ## git linear fit without outliers
      linmod <- try(lm(eor ~ deficit, data = mct_fluxnet$df))
      
      if (class(linmod) != "try-error"){
        
        ## test: is slope negative?
        is_neg <- coef(linmod)["deficit"] < 0.0
        
        if (is_neg){
          ## test: is slope significantly (5% level) different from zero (t-test)?
          is_sign <- coef(summary(linmod))["deficit", "Pr(>|t|)"] < 0.05
          if (is_sign){
            ## get x-axis cutoff
            eor0_fluxnet <- - coef(linmod)["(Intercept)"] / coef(linmod)["deficit"]
            df_fit <- tibble(y = predict(linmod, newdata = mct_fluxnet$df), x = mct_fluxnet$df$deficit)
          } else {
            eor0_fluxnet <- NA
          }
        } else {
          is_sign <- FALSE
          eor0_fluxnet <- NA
        }
      } else {
        is_sign <- FALSE
        eor0_fluxnet <- NA
      }
    } else {
      is_sign <- FALSE
      eor0_fluxnet <- NA
    }
    
    ## get exponential fit
    expmod <- try( nls(eor ~ a * exp(k * deficit), data = mct_fluxnet$df, start = list(a = 0.1, k = 1/50)) )
    if (class(expmod) != "try-error"){
      
      df_coef <- summary(expmod) %>% coef()
      
      ## test: is slope negative?
      is_neg_exp <- df_coef["k", "Estimate"] < 0

      if (is_neg_exp){
        ## test: is slope significantly (5% level) different from zero (t-test)?
        is_sign_exp <- df_coef["k", "Pr(>|t|)"] < 0.05
        
        if (is_sign_exp){
          
          ## get CWD at wich LUE is at 1/2e
          eor0_exp_fluxnet <- -1.0/coef(expmod)["k"]*2.0   ## CWD where eor is reduced to 1/2e
          df_fit_exp <- tibble(y = predict(expmod, newdata = mct_fluxnet$df), x = mct_fluxnet$df$deficit)
          
        } else {
          eor0_exp_fluxnet <- NA
        }
      } else {
        is_sign_exp <- FALSE
        eor0_exp_fluxnet <- NA
      }
      
    } else {
      is_sign_exp <- FALSE
      eor0_exp_fluxnet <- NA
    }
    
    ## plot
    out <- mct_fluxnet$df %>% 
      ggplot(aes(x = deficit, y = eor)) +
      geom_point(alpha = 0.5) +
      labs(title = sitename, x = "Cumulative water deficit (mm)", y = "ET/Rn", subtitle = "ET: FLUXNET, precipitation: FLUXNET, Rn: FLUXNET") +
      ylim(-0.1, 0.5)

    if (is_sign){
      out <- out +
        geom_vline(xintercept = eor0_fluxnet, linetype = "dotted", color = 'tomato') +
        geom_line(data = df_fit, aes(x, y), col = "tomato")
    }  
    if (is_sign_exp){
      out <- out +
        geom_vline(xintercept = eor0_exp_fluxnet, linetype = "dotted", color = 'royalblue') +
        geom_line(data = df_fit_exp, aes(x, y), col = "royalblue")
    }
    if (!identical(gumbi_fluxnet$mod, NA)){
      mctXX_fluxnet <- gumbi_fluxnet$df_return %>% dplyr::filter(return_period == use_return_period) %>% pull(return_level)
      if (!is.na(mctXX_fluxnet)){
        out <- out +
          geom_vline(xintercept = mctXX_fluxnet)
      } else {
        rlang::warn(paste("No FLUXNET MCT outputs for site", sitename))
      }
    } else {
      rlang::warn(paste("No FLUXNET MCT outputs for site", sitename))
      mctXX_fluxnet <- NA
    }
    out
    ggsave(paste0("fig/eor_cwd_fluxnet/eor_cwd_fluxnet_", sitename, ".pdf"), width = 6, height = 4)

  
  ## EOR vs CWD: ALEXI/WATCH-WFDEI------------------------------------------
    ## retain only data from largest instances of each year
    biginstances <- mct_alexi$inst %>% 
      mutate(year = lubridate::year(date_start)) %>% 
      group_by(year) %>% 
      dplyr::filter(deficit == max(deficit)) %>% 
      pull(iinst)

    mct_alexi$df <- mct_alexi$df %>% 
      left_join(mct_fluxnet$df %>% 
                  dplyr::select(date, netrad),
                by = "date") %>%
      dplyr::filter(iinst %in% biginstances) %>%
      mutate(eor = et/netrad) 
    
    ## get linear fit with outliers
    linmod <- try(lm(eor ~ deficit, data = mct_alexi$df))
    if (class(linmod) != "try-error"){
      idx_drop <- which(is.na(remove_outliers(linmod$residuals, coef = 1.5)))
      mct_alexi$df$eor[idx_drop] <- NA
      
      ## git linear fit without outliers
      linmod <- try(lm(eor ~ deficit, data = mct_alexi$df))
      
      if (class(linmod) != "try-error"){
        ## test: is slope negative?
        is_neg <- coef(linmod)["deficit"] < 0.0
        
        if (is_neg){
          ## test: is slope significantly (5% level) different from zero (t-test)?
          is_sign <- coef(summary(linmod))["deficit", "Pr(>|t|)"] < 0.05
          if (is_sign){
            ## get x-axis cutoff
            eor0_alexi <- - coef(linmod)["(Intercept)"] / coef(linmod)["deficit"]
            df_fit <- tibble(y = predict(linmod, newdata = mct_alexi$df), x = mct_alexi$df$deficit)
          } else {
            eor0_alexi <- NA
          }
        } else {
          is_sign <- FALSE
          eor0_alexi <- NA
        }
      } else {
        is_sign <- FALSE
        eor0_alexi <- NA
      }
    } else {
      is_sign <- FALSE
      eor0_alexi <- NA
    }
    
    ## get exponential fit
    expmod <- try( nls(eor ~ eor0 * exp(k * deficit), data = mct_alexi$df, start = list(eor0 = 0.1, k = 1/50)) )
    if (class(expmod) != "try-error"){
      
      df_coef <- summary(expmod) %>% coef()
      
      ## test: is slope negative?
      is_neg_exp <- df_coef["k", "Estimate"] < 0
      
      if (is_neg_exp){
        ## test: is slope significantly (5% level) different from zero (t-test)?
        is_sign_exp <- df_coef["k", "Pr(>|t|)"] < 0.05
        
        if (is_sign_exp){
          
          ## get CWD at wich eor is at 1/2e
          eor0_exp_alexi <- -1.0/coef(expmod)["k"]*2.0   ## CWD where eor is reduced to 1/2e
          df_fit_exp <- tibble(y = predict(expmod, newdata = mct_alexi$df), x = mct_alexi$df$deficit)
          
        } else {
          eor0_exp_alexi <- NA
        }
      } else {
        is_sign_exp <- FALSE
        eor0_exp_alexi <- NA
      }
      
    } else {
      is_sign_exp <- FALSE
      eor0_exp_alexi <- NA
    }
    
    out <- mct_alexi$df %>% 
      ggplot(aes(x = deficit, y = eor)) +
      geom_point(alpha = 0.5) +
      labs(title = sitename, x = "Cumulative water deficit (mm)", y = "ET/Rn", subtitle = "ET: ALEXI, precipitation: WATCH-WFDEI, Rn: FLUXNET") +
      ylim(-0.1, 0.5)
    
    if (is_sign){
      out <- out +
        geom_vline(xintercept = eor0_alexi, linetype = "dotted", color = 'tomato') +
        geom_line(data = df_fit, aes(x, y), col = "tomato")
    }
    if (is_sign_exp){
      out <- out +
        geom_vline(xintercept = eor0_exp_alexi, linetype = "dotted", color = 'royalblue') +
        geom_line(data = df_fit_exp, aes(x, y), col = "royalblue")
    }
    if (!identical(gumbi_alexi$mod, NA)){
      mctXX_alexi <- gumbi_alexi$df_return %>% dplyr::filter(return_period == use_return_period) %>% pull(return_level)
      if (!is.na(mctXX_alexi)){
        out <- out +
          geom_vline(xintercept = mctXX_alexi)
      } else {
        rlang::warn(paste("No ALEXI MCT outputs for site", sitename))
      }
    } else {
      rlang::warn(paste("No ALEXI MCT outputs for site", sitename))
      mctXX_alexi <- NA
    }
    out
    ggsave(paste0("fig/eor_cwd_alexi/eor_cwd_alexi_", sitename, ".pdf"), width = 6, height = 4)
      
  
  ## WATCH vs FLUXNET precipitation------------------------------------------
  out <- try(
    df_alexi %>%
      rename(sitename = idx, prec_watch = prec) %>%
      inner_join(
        df_fluxnet %>%
          rename(prec_fluxnet = prec),
        by = c("sitename", "date")
      ) %>%
      rbeni::analyse_modobs2("prec_fluxnet", "prec_watch")
  )
  if (class(out)!="try-error"){
    out$gg +
      labs(title = sitename, y = expression(paste("WATCH-WFDEI precipitation (mm d"^{-1}, ")")), x = expression(paste("FLUXNET precipitation (mm d"^{-1}, ")")))
    ggsave(paste0("fig/modobs_prec/modobs_prec_", sitename, ".pdf"), width = 5, height = 5)
  } else {
    rlang::warn(paste("Failed analyse_modobs2() for precip for site", sitename))
  }

  ## ... over time
  out <- df_alexi %>%
    rename(sitename = idx, prec_watch = prec) %>%
    inner_join(
      df_fluxnet %>%
        rename(prec_fluxnet = prec),
      by = c("sitename", "date")
    ) %>%
    pivot_longer(cols = c(prec_watch, prec_fluxnet), names_to = "source", values_to = "prec") %>%
    ggplot(aes(x = date, y = prec, color = source)) +
    geom_line() +
    labs(title = sitename, y = expression(paste("Precipitation (mm d"^{-1}, ")")), x = "Date")
  out
  ggsave(paste0("fig/prec/prec_", sitename, ".pdf"), width = 6, height = 3)

  ## ALEXI vs FLUXNET ET
  out <- try(
    out <- df_alexi %>%
      rename(sitename = idx, prec_watch = prec) %>%
      inner_join(
        df_fluxnet %>%
          rename(prec_fluxnet = prec),
        by = c("sitename", "date")
      ) %>%
      rbeni::analyse_modobs2("latenth_mm", "et_mm", type = "heat")
      )
  if (class(out)!="try-error"){
    out$gg +
      labs(title = sitename, y = expression(paste("ALEXI ET (mm d"^{-1}, ")")), x = expression(paste("FLUXNET ET (mm d"^{-1}, ")")))
    ggsave(paste0("fig/modobs_et/modobs_et_", sitename, ".pdf"), width = 5, height = 5)
  } else {
    rlang::warn(paste("Failed analyse_modobs2() for ET for site", sitename))
  }

  ## ... over time
  out <- df_alexi %>%
    dplyr::select(-et) %>%
    rename(sitename = idx, prec_watch = prec, et_alexi = et_mm) %>%
    inner_join(
      df_fluxnet %>%
        rename(prec_fluxnet = prec, et_fluxnet = latenth_mm),
      by = c("sitename", "date")
    ) %>%
    pivot_longer(cols = c(et_alexi, et_fluxnet), names_to = "source", values_to = "et") %>%
    ggplot(aes(x = date, y = et, color = source)) +
    geom_line() +
    labs(title = sitename, y = expression(paste("ET (mm d"^{-1}, ")")), x = "Date")
  out
  ggsave(paste0("./fig/et/et_", sitename, ".pdf"), width = 6, height = 3)

  ## Plot the distribution of cumulative deficits and the extreme values------------------------------------------
  out <- ggplot() +
    geom_histogram(
      data = mct_fluxnet$inst,
      aes(x = deficit, y = ..density..),
      color = "black", alpha = 0.5, fill = "black",
      position="identity") +
    geom_histogram(
      data = mct_alexi$inst,
      aes(x = deficit, y = ..density..),
      color = "black", alpha = 0.5, fill = "tomato", 
      position="identity") +
    labs(title = sitename, x = "Cumulative water deficit (mm)")
  
  if (!identical(gumbi_fluxnet$mod, NA)){
    mctXX_fluxnet <- gumbi_fluxnet$df_return %>% dplyr::filter(return_period == use_return_period) %>% pull(return_level)
    if (!is.na(mctXX_fluxnet)){
      out +
        geom_vline(xintercept = mctXX_fluxnet, col = "black")
    } else {
      rlang::warn(paste("No FLUXNET MCT outputs for site", sitename))
    }
  } else {
    rlang::warn(paste("No FLUXNET MCT outputs for site", sitename))
  }
  if (!identical(gumbi_alexi$mod, NA)){
    mctXX_alexi <- gumbi_alexi$df_return %>% dplyr::filter(return_period == use_return_period) %>% pull(return_level)
    if (!is.na(mctXX_alexi)){
      out +
        geom_vline(xintercept = mctXX_alexi, col = "tomato")
    } else {
      rlang::warn(paste("No ALEXI MCT outputs for site", sitename))
    }
  } else {
    rlang::warn(paste("No ALEXI MCT outputs for site", sitename))
  }
  out
  ggsave(paste0("./fig/hist/hist_cwd_", sitename, ".pdf"), width = 6, height = 3)
  

  ## Output------------------------------------------
  return(
    list(
      mct_alexi = mct_alexi, mct_fluxnet = mct_fluxnet, 
      gumbi_fluxnet = gumbi_fluxnet, gumbi_alexi = gumbi_alexi, 
      mctXX_fluxnet = mctXX_fluxnet, mctXX_alexi = mctXX_alexi, 
      lue0_fluxnet = lue0_fluxnet, lue0_exp_fluxnet = lue0_exp_fluxnet, 
      lue0_alexi = lue0_alexi, lue0_exp_alexi = lue0_exp_alexi )
    )
  
}