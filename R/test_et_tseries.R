test_et_tseries <- function(df, sitename = NA, filter_years = NA){

	##------------------------------------------
	## Time series of ET
	##------------------------------------------
  if (!is.na(filter_years)){
    df <- df %>% 
      mutate(year = lubridate::year(date)) %>% 
      dplyr::filter(year %in% filter_years)
  }
  
	gg <- ggplot() +
	  geom_line(data = df, aes(date, prec), color="royalblue") +
	  geom_line(data = df, aes(date, et_mm), color="tomato") +
	  theme_classic() +
	  labs(subtitle = "ET: ALEXI, precipitation: WATCH-WFDEI", x = "Date", y = "Cumulative water deficit (mm)")
	
	if (!is.na(sitename)){
	  gg <- gg +
	    labs(title = sitename)
	}
	
	#ggsave(paste0(dir, "/cwd/cwd_", sitename, ".pdf"), width = 6, height = 3)

	return(gg)

}