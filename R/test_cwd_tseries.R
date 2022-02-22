test_cwd_tseries <- function(out_mct, sitename = NA, filter_years = NA, title = NULL){

  if ("data.frame" %in% class(out_mct$df)){
    ##------------------------------------------
    ## Time series of cumulative deficits and events
    ##------------------------------------------
    maxval <- max(out_mct$df$deficit, na.rm = TRUE) %>% 
      ifelse(is.infinite(.), NA, .)
    
    if (!is.na(filter_years)){
      out_mct$inst <- out_mct$inst %>% 
        mutate(year_start = lubridate::year(date_start)) %>% 
        dplyr::filter(year_start %in% filter_years)
      
      out_mct$df <- out_mct$df %>% 
        mutate(year = lubridate::year(date)) %>% 
        dplyr::filter(year %in% filter_years)
    }
    
    # biginstances <- out_mct$inst %>% 
    #   mutate(year = lubridate::year(date_start)) %>% 
    #   group_by(year) %>% 
    #   dplyr::filter(deficit == max(deficit)) %>% 
    #   pull(iinst)
    # 
    # tmp <- out_mct$df %>% 
    #   dplyr::filter(iinst %in% biginstances)
    
    tmp <- out_mct$df %>% 
      mutate(year = lubridate::year(time)) %>% 
      group_by(year) %>% 
      summarise(cwdmax = max(deficit, na.rm = TRUE)) %>% 
      pull(cwdmax)
    
    out_mct$df <- out_mct$df %>% 
      mutate(deficit_ismax = ifelse(deficit %in% tmp, deficit, NA))
    
    gg <- ggplot() +
      geom_rect(
        data=out_mct$inst,
        aes(xmin=date_start, xmax=date_end, ymin=-99, ymax=99999),
        fill=rgb(0,0,0,0.2),
        color=NA) +
      # geom_line(data = out_mct$df, aes(date, liquid_to_soil), color="royalblue") +
      # geom_line(data = out_mct$df, aes(date, et_mm), color="springgreen3") +
      geom_line(data = out_mct$df, aes(time, deficit), color="tomato") +
      geom_point(data = out_mct$df, aes(time, deficit_ismax)) +
      coord_cartesian(ylim=c(0, maxval)) +
      theme_classic() +
      labs(subtitle = title, x = "Date", y = "CWD (mm)")
    
    if (!is.na(sitename)){
      gg <- gg +
        labs(title = sitename)
    }
    
    #ggsave(paste0(dir, "/cwd/cwd_", sitename, ".pdf"), width = 6, height = 3)
    
  } else {
    gg <- NULL
  }
	return(gg)

}