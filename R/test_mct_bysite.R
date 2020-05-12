test_mct_bysite <- function(sitename, df, thresh_terminate = 0.2, thresh_drop = 0.8, use_return_period = 10, fittype = NULL, dir = "."){
  
  print(sitename)

  if (!dir.exists(paste0(dir, "/cwd"))) system(paste0("mkdir -p ", dir, "/cwd"))
  if (!dir.exists(paste0(dir, "/lue_cwd"))) system(paste0("mkdir -p ", dir, "/lue_cwd"))
  if (!dir.exists(paste0(dir, "/hist"))) system(paste0("mkdir -p ", dir, "/hist"))
  if (!dir.exists(paste0(dir, "/prec"))) system(paste0("mkdir -p ", dir, "/prec"))
  if (!dir.exists(paste0(dir, "/modobs_prec"))) system(paste0("mkdir -p ", dir, "/modobs_prec"))
  if (!dir.exists(paste0(dir, "/modobs_et"))) system(paste0("mkdir -p ", dir, "/modobs_et"))
  if (!dir.exists(paste0(dir, "/et"))) system(paste0("mkdir -p ", dir, "/et"))
  
  ##------------------------------------------
  ## calculate daily water balance
  ##------------------------------------------
  df <- df %>% 
    unnest(df) %>% 
    mutate(et_mm = ifelse(is.na(et_mm), 0, et_mm)) %>% 
    mutate(bal = prec - et_mm) %>% 
    mutate(bal = myapprox(bal)) %>% 
    drop_na(bal)
  
  ##------------------------------------------
  ## apply mct function
  ##------------------------------------------
  out_mct <- mct(df, varname_wbal = "bal", thresh_terminate = thresh_terminate, thresh_drop = thresh_drop )

  ##------------------------------------------
  ## fit extreme value statistics  
  ##------------------------------------------
  gumbi <- get_plantwhc_mct_bysite(df,   varname_wbal = "bal", thresh_terminate = thresh_terminate, thresh_drop = thresh_drop, fittype = fittype)
  
  ##------------------------------------------
  ## Time series of cumulative deficits and events
  ##------------------------------------------
  ggplot() +
    geom_rect(
      data=out_mct$inst,
      aes(xmin=date_start, xmax=date_end, ymin=-99, ymax=99999),
      fill=rgb(0,0,0,0.3),
      color=NA) +
    geom_line(data = out_mct$df, aes(date, prec), size = 0.2, color="royalblue") +
    geom_line(data = out_mct$df, aes(date, deficit), color="tomato", size = 0.2) +
    geom_line(data = out_mct$df, aes(date, 100 * (gpp/ppfd)), size = 0.2) +
    coord_cartesian(ylim=c(0, max(out_mct$df$deficit, na.rm = TRUE))) +
    theme_classic() +
    labs(title = sitename, subtitle = "ET: ALEXI, precipitation: WATCH-WFDEI", x = "Date", y = "Cumulative water deficit (mm)")
  
  ggsave(paste0(dir, "/cwd/cwd_", sitename, ".pdf"), width = 6, height = 3)
  
  ##------------------------------------------
  ## Time series of evapotranspiration
  ##------------------------------------------
  df %>%
    ggplot(aes(x = date, y = et_mm, color = source)) +
    geom_line() +
    labs(title = sitename, y = expression(paste("ET (mm d"^{-1}, ")")), x = "Date")
  ggsave(paste0("./fig/et/et_", sitename, ".pdf"), width = 6, height = 3)
  
  ##------------------------------------------
  ## LUE vs CWD
  ##------------------------------------------
  ## retain only data from largest instances of each year
  biginstances <- out_mct$inst %>% 
    mutate(year = lubridate::year(date_start)) %>% 
    group_by(year) %>% 
    filter(deficit == max(deficit)) %>% 
    pull(iinst)

  out_mct$df <- out_mct$df %>% 
    mutate(lue = gpp/ppfd) 
  
  ## get linear fit with outliers
  linmod <- try(lm(lue ~ deficit, data = out_mct$df))
  if (class(linmod) != "try-error"){
    idx_drop <- which(is.na(remove_outliers(linmod$residuals, coef = 1.5)))
    out_mct$df$lue[idx_drop] <- NA
    
    ## git linear fit without outliers
    linmod <- try(lm(lue ~ deficit, data = out_mct$df))
    
    if (class(linmod) != "try-error"){
      ## test: is slope negative?
      is_neg <- coef(linmod)["deficit"] < 0.0
      
      if (is_neg){
        ## test: is slope significantly (5% level) different from zero (t-test)?
        is_sign <- coef(summary(linmod))["deficit", "Pr(>|t|)"] < 0.05
        if (is_sign){
          ## get x-axis cutoff
          lue0 <- - coef(linmod)["(Intercept)"] / coef(linmod)["deficit"]
          df_fit <- tibble(y = predict(linmod, newdata = out_mct$df), x = out_mct$df$deficit)
        } else {
          lue0 <- NA
        }
      } else {
        is_sign <- FALSE
        lue0 <- NA
      }
    } else {
      is_sign <- FALSE
      lue0 <- NA
    }
  } else {
    is_sign <- FALSE
    lue0 <- NA
  }
  
  ## get exponential fit
  expmod <- try( nls(lue ~ lue0 * exp(k * deficit), data = out_mct$df, start = list(lue0 = 0.1, k = 1/50)) )
  if (class(expmod) != "try-error"){
    
    df_coef <- summary(expmod) %>% coef()
    
    ## test: is slope negative?
    is_neg_exp <- df_coef["k", "Estimate"] < 0
    
    if (is_neg_exp){
      ## test: is slope significantly (5% level) different from zero (t-test)?
      is_sign_exp <- df_coef["k", "Pr(>|t|)"] < 0.05
      
      if (is_sign_exp){
        
        ## get CWD at wich LUE is at 1/2e
        lue0_exp <- -1.0/coef(expmod)["k"]*2.0   ## CWD where LUE is reduced to 1/2e
        df_fit_exp <- tibble(y = predict(expmod, newdata = out_mct$df), x = out_mct$df$deficit)
        
      } else {
        lue0_exp <- NA
      }
    } else {
      is_sign_exp <- FALSE
      lue0_exp <- NA
    }
    
  } else {
    is_sign_exp <- FALSE
    lue0_exp <- NA
  }
  
  out <- out_mct$df %>% 
    filter(iinst %in% biginstances) %>% 
    ggplot(aes(x = deficit, y = lue)) +
    geom_point(alpha = 0.5) +
    labs(title = sitename, x = "Cumulative water deficit (mm)", y = "GPP/PPFD", subtitle = "ET: ALEXI, precipitation: WATCH-WFDEI, GPP: FLUXNET, PPFD: FLUXNET") +
    ylim(-0.1, 0.5)
  
  if (is_sign){
    out <- out +
      geom_vline(xintercept = lue0, linetype = "dotted", color = 'tomato') +
      geom_line(data = df_fit, aes(x, y), col = "tomato")
  }
  if (is_sign_exp){
    out <- out +
      geom_vline(xintercept = lue0_exp, linetype = "dotted", color = 'royalblue') +
      geom_line(data = df_fit_exp, aes(x, y), col = "royalblue")
  }
  if (!identical(gumbi$mod, NA)){
    mctXX <- gumbi$df_return %>% filter(return_period == use_return_period) %>% pull(return_level)
    if (!is.na(mctXX)){
      out <- out +
        geom_vline(xintercept = mctXX)
    } else {
      rlang::warn(paste("No ALEXI MCT outputs for site", sitename))
    }
  } else {
    rlang::warn(paste("No ALEXI MCT outputs for site", sitename))
    mctXX <- NA
  }
  out
  ggsave(paste0(dir, "/lue_cwd/lue_cwd_", sitename, ".pdf"), width = 6, height = 4)
  

  ##------------------------------------------
  ## Histogram: Distribution of cumulative deficits and the extreme values
  ##------------------------------------------
  out <- ggplot() +
    geom_histogram(
      data = out_mct$inst,
      aes(x = deficit, y = ..density..),
      color = "black", alpha = 0.5, fill = "tomato", 
      position="identity") +
    labs(title = sitename, x = "Cumulative water deficit (mm)")
  
  if (!identical(gumbi$mod, NA)){
    mctXX <- gumbi$df_return %>% filter(return_period == use_return_period) %>% pull(return_level)
    if (!is.na(mctXX)){
      out +
        geom_vline(xintercept = mctXX, col = "tomato")
    } else {
      rlang::warn(paste("No ALEXI MCT outputs for site", sitename))
    }
  } else {
    rlang::warn(paste("No ALEXI MCT outputs for site", sitename))
  }
  out
  ggsave(paste0(dir, "/hist/hist_cwd_", sitename, ".pdf"), width = 6, height = 3)
  

  ##------------------------------------------
  ## Output
  ##------------------------------------------
  return(
    list(
      mct = mct, 
      gumbi = gumbi, 
      mctXX = mctXX, 
      lue0 = lue0, lue0_exp = lue0_exp )
    )
  
}