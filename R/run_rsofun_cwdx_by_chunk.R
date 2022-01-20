run_rsofun_cwdx_by_chunk <- function(ichunk){

	source("R/process_pmodel.R")

	## load forcing data
	df_forcing <- readRDS("./data/forcing_rsip/pmodel_drivers_1.rds")

	## run rsofun with WHC = S_CWDX80
	df_output <- process_pmodel(
	  df_forcing,
	  agg = FALSE
	  )

	## run rsofun with WHC = 100
	df_forcing <- df_forcing

	df_output <- process_pmodel(
	  df_forcing,
	  agg = FALSE
	  )

	## run rsofun with WHC = 200
	df_forcing <- df_forcing

	df_output <- process_pmodel(
	  df_forcing,
	  agg = FALSE
	  )

	## nest
	df_output <- df_output %>%
	  group_by(sitename) %>%
	  nest() %>% 
	  mutate(data, purrr::map(data, ~mutate(., 
	                                        wbal = infilt - transp,
	                                        ef = transp / pet)))
	  
	## get CWD time series and events
	out_mct <- mct(list_output[[1]], 
	               varname_wbal = "wbal", 
	               varname_date = "date" 
	               )

	## get S_CWDX80
	gumbi <- get_plantwhc_mct_bysite(list_output[[1]] %>% 
	                                   dplyr::select(-date_start, -date_end),   
	                                 varname_wbal = "wbal", 
	                                 varname_date = "date" 
	                                 )

	## diagnose S_dEF from model outputs
	out <- calc_cwd_lue0(out_mct$df, 
	              out_mct$inst, 
	              "ef", 
	              do_plot = TRUE, 
	              verbose = TRUE
	              )

	## plot 
	out$gg +
	  geom_vline(xintercept = df_forcing$site_info[[1]]$whc) +
	  geom_vline(xintercept = gumbi$df_return %>% 
	                            filter(return_period == use_return_period) %>% 
	                            pull(return_level),
	             color = "royalblue"
	             )

	  
}