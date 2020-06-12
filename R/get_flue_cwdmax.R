get_flue_cwdmax <- function(df, do_plot = FALSE){

	nbin <- 5

	# ggplot(data = df, aes(deficit, lue)) + geom_point()
	
	df <- df %>%
		mutate(bin = ntile(deficit, nbin))

	df_agg <- df %>%
		ungroup() %>%
		group_by(bin) %>%
		summarise(lue = median(lue, na.rm = TRUE))			

	flue <- (df_agg %>% slice(nbin) %>%  pull(lue)) / (df_agg %>% slice(1) %>%  pull(lue))

	cwdmax <- max(df$deficit, na.rm = TRUE)

	return(list(flue = flue, cwdmax = cwdmax))

}