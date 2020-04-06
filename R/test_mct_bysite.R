test_mct_bysite <- function(sitename, df_fluxnet, df_alexi, thresh_terminate = 0.2, thresh_drop = 0.8, use_return_period = 10){
  
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
  
  ## apply mct function
  mct_fluxnet <- mct(df_fluxnet, varname_wbal = "bal", thresh_terminate = thresh_terminate, thresh_drop = thresh_drop )
  mct_alexi   <- mct(df_alexi,   varname_wbal = "bal", thresh_terminate = thresh_terminate, thresh_drop = thresh_drop )

  ## fit extreme value statistics  
  gumbi_alexi   <- get_plantwhc_mct_bysite(df_alexi,   varname_wbal = "bal")
  gumbi_fluxnet <- get_plantwhc_mct_bysite(df_fluxnet, varname_wbal = "bal")
  
  if (!dir.exists("fig/cwd_fluxnet")) system("mkdir -p fig/cwd_fluxnet")
  if (!dir.exists("fig/cwd_alexi")) system("mkdir -p fig/cwd_alexi")
  if (!dir.exists("fig/cwd_alexi_fluxnet")) system("mkdir -p fig/cwd_alexi_fluxnet")
  if (!dir.exists("fig/lue_cwd_fluxnet")) system("mkdir -p fig/lue_cwd_fluxnet")
  if (!dir.exists("fig/lue_cwd_alexi")) system("mkdir -p fig/lue_cwd_alexi")
  if (!dir.exists("fig/hist")) system("mkdir -p fig/hist")
  if (!dir.exists("fig/prec")) system("mkdir -p fig/prec")
  if (!dir.exists("fig/modobs_prec")) system("mkdir -p fig/modobs_prec")
  if (!dir.exists("fig/modobs_et")) system("mkdir -p fig/modobs_et")
  if (!dir.exists("fig/et")) system("mkdir -p fig/er")
  
  ## cumulative deficits
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
  ggplot() +
    geom_line(data = mct_fluxnet$df, aes(date, deficit), color="black", size = 0.2) +
    geom_line(data = mct_alexi$df, aes(date, deficit), color="tomato", size = 0.2) +
    labs(title = sitename, subtitle = "ALEXI/WATCH vs FLUXNET", x = "Date", y = "Cumulative water deficit (mm)")
  ggsave(paste0("fig/cwd_alexi_fluxnet/cwd_alexi_fluxnet_", sitename, ".pdf"), width = 6, height = 3)
  
  
  ## LUE vs CWD: FLUXNET
  nyears <- mct_fluxnet$df %>% 
    mutate(year = year(date)) %>% 
    pull(year) %>% 
    unique() %>% 
    length()
  biginstances <- mct_fluxnet$inst %>% 
    arrange(-deficit) %>% 
    slice(1:nyears) %>% 
    pull(iinst)
  
  out <- mct_fluxnet$df %>% 
    filter(iinst %in% biginstances) %>% 
    ggplot(aes(x = deficit, y = gpp/ppfd)) +
    geom_point(alpha = 0.5) +
    labs(title = sitename, x = "Cumulative water deficit (mm)", y = "GPP/PPFD", subtitle = "ET: FLUXNET, precipitation: FLUXNET, GPP: FLUXNET, PPFD: FLUXNET") +
    geom_smooth(aes(y = gpp/ppfd), color = 'red', method = 'lm', se = TRUE)
  if (!identical(gumbi_fluxnet, NA)){
    mct20 <- gumbi_fluxnet %>% filter(return_period == use_return_period) %>% pull(return_level)
    if (!is.na(mct20)){
      out +
        geom_vline(xintercept = mct20, col = "black")
    } else {
      rlang::warn(paste("No FLUXNET MCT outputs for site", sitename))
    }
  } else {
    rlang::warn(paste("No FLUXNET MCT outputs for site", sitename))
  }
  out
  ggsave(paste0("fig/lue_cwd_fluxnet/lue_cwd_fluxnet_", sitename, ".pdf"), width = 6, height = 4)
  
  
  ## LUE vs CWD: ALEXI/WATCH-WFDEI
  nyears <- mct_alexi$df %>% 
    mutate(year = year(date)) %>% 
    pull(year) %>% 
    unique() %>% 
    length()
  biginstances <- mct_alexi$inst %>% 
    arrange(-deficit) %>% 
    slice(1:nyears) %>% 
    pull(iinst)
  
  out <- mct_alexi$df %>% 
    filter(iinst %in% biginstances) %>% 
    ggplot(aes(x = deficit, y = gpp/ppfd)) +
    geom_point(alpha = 0.5) +
    labs(title = sitename, x = "Cumulative water deficit (mm)", y = "GPP/PPFD", subtitle = "ET: ALEXI, precipitation: WATCH-WFDEI, GPP: FLUXNET, PPFD: FLUXNET") +
    geom_smooth(aes(y =  gpp/ppfd), color = 'red', method = 'lm', se = TRUE)
  if (!identical(gumbi_alexi, NA)){
    mct20 <- gumbi_alexi %>% filter(return_period == use_return_period) %>% pull(return_level)
    if (!is.na(mct20)){
      out +
        geom_vline(xintercept = mct20, col = "tomato")
    } else {
      rlang::warn(paste("No ALEXI MCT outputs for site", sitename))
    }
  } else {
    rlang::warn(paste("No ALEXI MCT outputs for site", sitename))
  }
  out
  ggsave(paste0("fig/lue_cwd_alexi/lue_cwd_alexi_", sitename, ".pdf"), width = 6, height = 4)
  
  
  ## WATCH vs FLUXNET precipitation
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

  ## Plot the distribution of cumulative deficits and the extreme values
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
  
  if (!identical(gumbi_fluxnet, NA)){
    mct20 <- gumbi_fluxnet %>% filter(return_period == use_return_period) %>% pull(return_level)
    if (!is.na(mct20)){
      out +
        geom_vline(xintercept = mct20, col = "black")
    } else {
      rlang::warn(paste("No FLUXNET MCT outputs for site", sitename))
    }
  } else {
    rlang::warn(paste("No FLUXNET MCT outputs for site", sitename))
  }
  if (!identical(gumbi_alexi, NA)){
    mct20 <- gumbi_alexi %>% filter(return_period == use_return_period) %>% pull(return_level)
    if (!is.na(mct20)){
      out +
        geom_vline(xintercept = mct20, col = "tomato")
    } else {
      rlang::warn(paste("No ALEXI MCT outputs for site", sitename))
    }
  } else {
    rlang::warn(paste("No ALEXI MCT outputs for site", sitename))
  }
  out
  ggsave(paste0("./fig/hist/hist_cwd_", sitename, ".pdf"), width = 6, height = 3)
  

  return(mct_alexi)
  
}