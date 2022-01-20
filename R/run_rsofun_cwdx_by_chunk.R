run_rsofun_cwdx_by_chunk <- function(ichunk, use_whc = NA){

	## load forcing data
	df_forcing <- readRDS(paste0("./data/forcing_rsip/pmodel_drivers_", ichunk, ".rds"))

	## overwrite WHC to diagnose it later as a test
	if (!is.na(use_whc)){
	  df_forcing <- df_forcing %>% 
	    mutate(site_info = purrr::map(site_info, ~mutate(., whc = use_whc)))
	}
	
	## run rsofun with WHC = S_CWDX80
	df_output <- process_pmodel(
	  df_forcing,
	  agg = FALSE
	  ) %>% 

  	## nest
	  group_by(sitename) %>%
	  nest() %>% 
	  
	  ## add derived variables
	  mutate(data = purrr::map(data, ~mutate(., 
	                                         wbal = infilt - transp,
	                                         ef = transp / pet))) %>% 
	  
  	## get S_CWDX80, get CWD time series and events
	  # ungroup() %>%
	  # slice(1:2) %>%
	  mutate(gumbi = purrr::map(data, ~get_plantwhc_mct_bysite(., 
	                                         varname_wbal = "wbal", 
	                                         varname_date = "date" ))) %>% 
	
  	## diagnose S_dEF from model outputs
	  mutate(out_mct = purrr::map(gumbi, "mct")) %>% 
	  
	  ## extract s_cwdx80
	  mutate(s_cwdx80 = purrr::map_dbl(gumbi, ~extract_return_level(., 80))) %>% 
	  dplyr::select(-gumbi) %>% 
	  mutate(df = purrr::map(out_mct, "df")) %>% 
	  mutate(inst = purrr::map(out_mct, "inst")) %>% 
	  dplyr::select(-out_mct) %>% 
	  mutate(out_cwd_lue0 = purrr::map2(df, inst, ~calc_cwd_lue0(.x, .y, 
	                                                             "ef", 
	                                                             do_plot = TRUE, 
	                                                             verbose = TRUE))) %>% 
	  dplyr::select(-df, -inst) %>% 

	  ## extract diagnosed S0: s_def
	  mutate(s_def = purrr::map_dbl(out_cwd_lue0, "cwd_lue0")) %>% 
	  
	  ## complement data frame with prescribed S0 (whc)
	  left_join(
	    df_forcing %>% 
	      dplyr::select(sitename, site_info) %>% 
	      unnest(site_info) %>% 
	      dplyr::select(sitename, whc),
	    by = "sitename"
	  )
	
	# ## plot
	# tmp %>% analyse_modobs2("whc", "s_def")

	return(df_output)
}