#' Plot a nice map with ggplot.
#'
#' Returns a cowplot object for a global map plot.
#'
#' @param df An object, either a \code{RasterBrick} (returned from a \code{raster::brick()} function call),
#' or a list returned from a \code{rbeni::read_nc_onefile()} function call.
#' @param df_irr Data frame with irrigation fraction
#' @param nbin An integer specifying the number of bins used for the color key.
#' @param maxval A numeric value specifying the maximum value for which the color key is to be extended. Defaults
#' to \code{NA} (the 99\% quantile of values is used).
#' @param breaks A numeric vector specifying the breaks for the color scale. Defaults to \code{NA}, i.e. breaks
#' are determined automatically based on \code{nbin} and \code{maxval}.
#' @param lonmin Left edge (longitude, in degrees), defaults to -180.
#' @param lonmax Right edge (longitude, in degrees), defaults to 180.
#' @param latmin Lower edge (latitude, in degrees), defaults to -90.
#' @param latmax Upper edge (latitude, in degrees), defaults to 90.
#' @param plot_title A character string specifying the plot title
#' @param plot_subtitle A character string specifying the plot subtitle
#' @param legend_title A character string specifying the legend title (annotation above the color key)
#' @param colorscale Either function that returns a set of colors or a vector of color names from which to interpolate.
#' Defaults to \code{virids::viridis}.
#' @param do_reproj A boolean specifying whether to re-project the map to Robin projection
#' @param hillshade A logical specifying whether a hillshade layer should be added. Defaults to \code{FALSE}.
#' @param rivers A logical specifying whether to display rivers (the \code{ne_50m_rivers_lake_centerlines} layer from NaturalEarth.). Defaults to \code{FALSE}.
#' @param lakes A logical specifying whether to display rivers (the \code{ne_50m_lakes} layer from NaturalEarth). Defaults to \code{FALSE}.
#' @param coast A logical specifying whether to display coastlines (the \code{ne_50m_coastline} layer from NaturalEarth). Defaults to \code{TRUE}.
#' @param scale A character string specifying the scale of geo layers (coast, rivers, lakes). One of \code{"low", "medium", "high"}. 
#' NaturalEarth layers for 110, 50, 10 m are used for low, medium, and high resolution (scale) layers, respectively. Defaults to \code{"low"}.
#' @param countries A logical specifying whether to display country borders (the \code{ne_50m_admin_0_countries} layer from NaturalEarth). Defaults to \code{FALSE}.
#' @param states A logical specifying whether to display sub-country administrative borders (e.g. US states) (the \code{ne_50m_admin_1_states_provinces} layer from NaturalEarth). Defaults to \code{FALSE}.
#' @param make_discrete A logical scpecifying whether data layer is to be made discrete for plotting with colors
#' of discrete bins. Defaults to \code{TRUE}.
#' @param combine A boolean specifying whether the map and the colorscale should be combined using cowplot.
#' Defaults to \code{TRUE}. If \code{FALSE}, a list of elements are retruned, where elements are the ggplot2 plot object
#' and the coloscale object returned by the call to \link{plot_discrete_cbar}.
#' @param varnam If \code{obj} is a rbeni-nc object (returned by \code{read_nc_onefile()}), \code{varnam} must be
#' provided (a character string specifying the variable name in \code{obj$vars[[varnam]]}).
#' @param irr_cutoff Cutoff for irrigation layer as a fraction of the total gridcell area
#'
#' @return A ggplot object for a global map plot.
#' @export
#'
plot_map_cwdx_type_irrigation <- function(
  df, df_irr,
  name_cwdx, name_type, name_irr,
  maxval = NA, breaks = NA, lonmin = -180, lonmax = 180, latmin = -90, latmax = 90,
  nbin = 10, legend_title = waiver(), colorscale = viridis::viridis, do_reproj = FALSE,
  hillshade = FALSE, rivers = FALSE, lakes = FALSE, coast = TRUE, countries = FALSE, 
  states = FALSE, scale = "low", make_discrete = TRUE,
  plot_title = waiver(), plot_subtitle = waiver(), combine = TRUE, varnam = NULL, irr_cutoff = 0.25, ...){

  library(rnaturalearth)
  library(sp)
  library(sf)
  library(ggplot2)
  library(rgdal)
  library(raster)
  library(ggnewscale)

  ## following https://downwithtime.wordpress.com/2013/12/04/naturalearthdata-and-r-in-ggplot2/

  ## read geo data
  res <- ifelse(scale == "low", "110", ifelse(scale == "medium", "50", ifelse(scale == "high", "10", NA)))
  if (!exists("raster_shade") && hillshade) raster_shade   <- raster::stack(paste0("~/data/naturalearth/SR_50M/SR_50M.tif"))
  if (!exists("layer_lakes") && lakes) layer_lakes         <- readOGR(paste0("~/data/naturalearth/ne_", res, "m_lakes/ne_", res, "m_lakes.shp"), paste0("ne_", res, "m_lakes"))
  if (!exists("layer_rivers") && rivers) layer_rivers      <- readOGR(paste0("~/data/naturalearth/ne_", res, "m_rivers_lake_centerlines/ne_", res, "m_rivers_lake_centerlines.shp"), paste0("ne_", res, "m_rivers_lake_centerlines"))
  if (!exists("layer_coast") && coast) layer_coast         <- readOGR(paste0("~/data/naturalearth/ne_", res, "m_coastline/ne_", res, "m_coastline.shp"), paste0("ne_", res, "m_coastline"))
	if (!exists("layer_country") && countries) layer_country <- readOGR(paste0("~/data/naturalearth/ne_", res, "m_countryline/ne_", res, "m_admin_0_countries.shp"), paste0("ne_", res, "m_admin_0_countries"))
	if (!exists("layer_states") && states) layer_states      <- readOGR(paste0("~/data/naturalearth/ne_", res, "m_admin_1_states_provinces/ne_", res, "m_admin_1_states_provinces.shp"), paste0("ne_", res, "m_admin_1_states_provinces"))

	##---------------------------------------------
	## object: is data frame
  ## reduce extent
	##---------------------------------------------
  ## is already a data frame. thanks.
  df <- as_tibble(df) %>%
    dplyr::filter(lon > lonmin & lon < lonmax & lat > latmin & lat < latmax) %>% 
    dplyr::select(lon, lat, cwdx = !!name_cwdx, type = !!name_type)
  
  df_irr <- df_irr %>% 
    dplyr::filter(lon > lonmin & lon < lonmax & lat > latmin & lat < latmax) %>% 
    dplyr::select(lon, lat, irr = !!name_irr)
  
	##---------------------------------------------
	## Bin CWDX data
	##---------------------------------------------
	toptriangle <- FALSE
	bottomtriangle <- FALSE

	if (identical(NA, breaks)){
	  breaks <- scales::pretty_breaks(n = nbin)(df$cwdx)
	  rlang::warn("Overwriting nbin after defining breaks with scales::pretty_breaks().")
	  nbin <- length(breaks) - 1
	} else {
	  nbin <- length(breaks) - 1
	}

	breaks_with <- breaks

	if (is.infinite(breaks[length(breaks)])){
	  toptriangle <- TRUE
	  breaks <- breaks[-(length(breaks)-1)]
	}
	if (is.infinite(breaks[1])){
	  bottomtriangle <- TRUE
	  breaks <- breaks[-2]
	}

	## update
	nbin <- length(breaks) - 1

	## bin CWDX data
	if (make_discrete){
	  df$cwdxcut <- as.factor(base::cut(df$cwdx, breaks=breaks, labels = FALSE, include.lowest = TRUE))
	} else {
	  df$cwdxcut <- df$cwdx
	}
	
	## irrigation cutoff
	df_irr <- df_irr %>% 
	  mutate(irr_bin = ifelse(irr > irr_cutoff, TRUE, NA))
	
	## create polygon of irrigation areas  https://stackoverflow.com/questions/34756755/plot-outline-around-raster-cells
	rasta <- rasterFromXYZ(df_irr %>% dplyr::select(x = lon, y = lat, z = irr_bin))
	pp <- rasterToPolygons(rasta, dissolve = TRUE)
	df_irr_outline <- fortify(pp) %>% as_tibble()
	
	# ggplot(df, aes(lon, lat)) +
	#   geom_raster(aes(fill = irr_bin)) +
	#   coord_fixed() +
	#   geom_path(aes(x = long, y = lat, group = group), data = df_irr_outline, size = 0.5, col = "royalblue")
	
	# # remove symbols from category-strings to make them nicer in the legend
	# df$cwdxcut <- gsub("\\(|\\]", "", df$cwdxcut)
	# df$cwdxcut <- gsub("\\,", " - ", df$cwdxcut)

	##---------------------------------------------
	## crop
	##---------------------------------------------
	domain <- c(lonmin, lonmax, latmin, latmax)

	if (lakes) lakes_crop <- mycrop(layer_lakes, domain)
	if (rivers) river_crop <- mycrop(layer_rivers, domain)
	if (coast) coast_crop <- mycrop(layer_coast, domain)
	if (countries) countries_crop <- mycrop(layer_countries, domain)
	if (states) states_crop <- mycrop(layer_states, domain)
	if (hillshade) raster_shade_crop <- crop(raster_shade, y=extent(domain))

	df <- df %>%
	  dplyr::filter(lon > domain[1] & lon < domain[2] & lat > domain[3] & lat < domain[4])

	## convert the hillshade layer to a data frame
	if (hillshade){
	  df_hs <- data.frame(
	    xyFromCell(raster_shade_crop, 1:ncell(raster_shade_crop)),
	    getValues(raster_shade_crop/255)) %>%
	    as_tibble()
	}

	##---------------------------------------------
	## Create color scale
	##---------------------------------------------
	if (class(colorscale)=="function"){

	  colorscale <- colorscale(nbin, direction = -1)

	} else if (class(colorscale)=="character"){

	  if (colorscale == "batlowK"){
	    colorscale <- scico::scico(nbin, palette = colorscale, direction = -1)
	  } else {
	    colorscale <- colorRampPalette( colorscale )( nbin )
	  }
	  
	} else if (class(colorscale)=="palette"){

	  ## nothing to do in this case
	  #colorscale <- colorscale

	} else {

	  rlang::abort("colorscale could not be set.")

	}

	if (toptriangle){
	  colorscale <- c(colorscale, colorscale[length(colorscale)])
	}
	if (bottomtriangle){
	  colorscale <- c(colorscale[1], colorscale)
	}

	##---------------------------------------------
	## Create ggplot object
	##---------------------------------------------
	ggmap <- ggplot() +

	  ## first layer: flattening
	  geom_tile(data = df, aes(x = lon, y = lat, fill = type, color = type)) +     # , show.legend = FALSE

	  ## color scale for type, colors from here: https://github.com/cj-holmes/vhs/blob/main/data-raw/create-palettes.R
	  scale_fill_manual( name = "Flattening", values = c("royalblue", "transparent"), labels = c("TRUE", "FALSE"), na.value = "transparent") +  # mauve: "#be7682ff"; orange: "#eb6133ff", green: "#00902eff"
	  scale_color_manual(name = "Flattening", values = c("royalblue", "transparent"), labels = c("TRUE", "FALSE"), na.value = "transparent") +
	  
  	## geoms below will use another color scale
	  new_scale_fill() +
	  new_scale_color() +
	  
	  ## second layer: CWDX
	  geom_tile(data = df, aes(x = lon, y = lat, fill = cwdxcut, color = cwdxcut), show.legend = FALSE, na.value = "transparent") +   # 

    scale_fill_manual(values = colorscale, na.value = "transparent") +
    scale_color_manual(values = colorscale, na.value = "transparent") +
	  
	  ## geoms below will use another color scale
	  new_scale_color() +
	  
	  ## third layer: irrigation outline of a polygon created above
	  geom_path(aes(x = long, y = lat, group = group, color = id), data = df_irr_outline, size = 0.5) +
	  
	  ## color scale for type, colors from here: https://github.com/cj-holmes/vhs/blob/main/data-raw/create-palettes.R
	  scale_color_manual(name = "Irrigated", values = "#dc3b39ff", labels = c("Outline", " ")) + # green: "#00902eff", red:  "#dc3b39ff"

	  scale_x_continuous(expand = c(0,0)) +
	  scale_y_continuous(expand = c(0,0)) +
	
	  xlab("Longitude (°E)") + ylab("Latitude (°N)") +
	  coord_sf() +
	  
	  theme_bw() +
	  theme(axis.ticks.y.right = element_line(),
	        axis.ticks.x.top = element_line(),
	        panel.grid = element_blank(),
	        axis.title = element_text(size = 10, color = "grey30"))

	## add coast layer
	if (coast){
	  ggmap <- ggmap +
	    geom_path(data = coast_crop, aes(x = long, y = lat, group = group), color = 'gray25', size = 0.1, linejoin = "mitre")
	}

	## add rivers layer
	if (rivers){
	  ggmap <- ggmap +
	    geom_path(data = river_crop, aes(x = long, y = lat, group = group), color = 'dodgerblue', size = 0.3)
	}

	## add lakes layer
	if (lakes){
	  ggmap <- ggmap +
	    geom_polygon(data = lakes_crop, aes(x = long, y = lat, group = group), fill = "#ADD8E6")
	}

	## add country layer
	if (countries){
	  ggmap <- ggmap +
	  	geom_path(data = countries_crop, aes(x = long, y = lat, group = group), color = 'gray25', size = 0.2)
	}

	## add states layer
	if (states){
	  ggmap <- ggmap +
	  	geom_path(data = states_crop, aes(x = long, y = lat, group = group), color = 'gray25', size = 0.1)
	}

	## add hillshade layer
	if (hillshade){
	  ggmap <- ggmap +
	    geom_tile(data = df_hs, aes(x = x, y = y, alpha = SR_50M), show.legend = FALSE) +
	    scale_alpha(range=c(0.5, 0))
	}

	gglegend <- plot_discrete_cbar(
		breaks           = breaks_with, # Vector of breaks. If +-Inf are used, triangles will be added to the sides of the color bar
		colors           = colorscale,
		legend_title     = legend_title,
		legend_direction = "vertical",
		...
    )

	if (combine){
	  out <- cowplot::plot_grid(ggmap, gglegend, ncol = 2, rel_widths = c(1, 0.2))
	} else {
	  out <- list(ggmap = ggmap, gglegend = gglegend)
	}

  return(out)
}


## Copied from https://github.com/adrfantini/plot_discrete_cbar

plot_discrete_cbar = function(
    breaks, # Vector of breaks. If +-Inf are used, triangles will be added to the sides of the color bar
    palette = "Greys", # RColorBrewer palette to use
    colors = RColorBrewer::brewer.pal(length(breaks) - 1, palette), # Alternatively, manually set colors
    direction = 1, # Flip colors? Can be 1 or -1
    spacing = "natural", # Spacing between labels. Can be "natural" or "constant"
    border_color = NA, # NA = no border color
    legend_title = NULL,
    legend_direction = "horizontal", # Can be "horizontal" or "vertical"
    font_size = 3.5,
    expand_size = 0, # Controls spacing around legend plot
    expand_size_y = 0.8,
    spacing_scaling = 0.3, # Multiplicative factor for label and legend title spacing
    width = 0.01, # Thickness of color bar
    triangle_size = 0.05 # Relative width of +-Inf triangles
    ) {

    require(ggplot2)

    if (!(spacing %in% c("natural", "constant"))) stop("spacing must be either 'natural' or 'constant'")
    if (!(direction %in% c(1, -1))) stop("direction must be either 1 or -1")
    if (!(legend_direction %in% c("horizontal", "vertical"))) stop("legend_direction must be either 'horizontal' or 'vertical'")
    breaks = as.numeric(breaks)
    new_breaks = sort(unique(breaks))
    if (any(new_breaks != breaks)) warning("Wrong order or duplicated breaks")
    breaks = new_breaks
    if (class(colors) == "function") colors = colors(length(breaks) - 1)
    if (length(colors) != length(breaks) - 1) stop("Number of colors (", length(colors), ") must be equal to number of breaks (", length(breaks), ") minus 1")
    if (!missing(colors)) warning("Ignoring RColorBrewer palette '", palette, "', since colors were passed manually")

    if (direction == -1) colors = rev(colors)

    inf_breaks = which(is.infinite(breaks))
    if (length(inf_breaks) != 0) breaks = breaks[-inf_breaks]
    plotcolors = colors

    n_breaks = length(breaks)

    labels = breaks

    if (spacing == "constant") {
        breaks = 1:n_breaks
    }

    r_breaks = range(breaks)
    d_breaks = breaks[2] - breaks[1]

    cbar_df = data.frame(stringsAsFactors = FALSE,
        y = breaks,
        yend = c(breaks[-1], NA),
        color = as.character(1:n_breaks)
    )[-n_breaks,]

    xmin = 1 - width/2
    xmax = 1 + width/2

    cbar_plot = ggplot(
      cbar_df,
      aes(xmin=xmin, xmax = xmax, ymin = y, ymax = yend, fill = factor(color, levels = 1:length(colors)))
      ) +
      geom_rect(show.legend = FALSE, color=border_color)

    ## Add arrows
    if (any(inf_breaks == 1)) { # Add < arrow for -Inf
        firstv = breaks[1]
        polystart = data.frame(
            x = c(xmin, xmax, 1),
            y = c(rep(firstv, 2), firstv - diff(r_breaks) * triangle_size)
        )
        plotcolors = plotcolors[-1]
        cbar_plot <- cbar_plot +
            geom_polygon(data=polystart, aes(x=x, y=y),
                        show.legend = FALSE,
                        inherit.aes = FALSE,
                        fill = colors[1],
                        color=border_color)
    }
    if (any(inf_breaks > 1)) { # Add > arrow for +Inf
        lastv = breaks[n_breaks]
        polyend = data.frame(
            x = c(xmin, xmax, 1),
            y = c(rep(lastv, 2), lastv + diff(r_breaks) * triangle_size)
        )
        plotcolors = plotcolors[-length(plotcolors)]
        cbar_plot <- cbar_plot +
            geom_polygon(data=polyend, aes(x=x, y=y),
                        show.legend = FALSE,
                        inherit.aes = FALSE,
                        fill = colors[length(colors)],
                        color=border_color)
    }

    if (legend_direction == "horizontal") {
        #horizontal legend
        mul = 1
        x = xmin
        xend = xmax
        cbar_plot <- cbar_plot + coord_flip()
        angle = 0
        legend_position = xmax + 0.1 * spacing_scaling

    } else {
        # vertical legend
        mul = -1
        x = xmax
        xend = xmin
        angle = -90
        legend_position = xmin # xmax + 0.2 * spacing_scaling
    }

    ymid <- (breaks[length(breaks)] + breaks[1]) / 2
    dy <- breaks[length(breaks)] - breaks[1]
    ybottom_abs <- ymid - dy/2 * 1/expand_size_y
    ytop_abs    <- ymid + dy/2 * 1/expand_size_y

    # Create color key
    cbar_plot <- cbar_plot +
        geom_segment(data = data.frame(y = breaks, yend = breaks),
            aes(y=y, yend=yend),
            x = x - 0.01 * mul * spacing_scaling, xend = x, #+ 0.01 * mul * spacing_scaling, # xend = xend,
            inherit.aes = FALSE) +
        annotate(geom = 'text', x = x - 0.02 * mul * spacing_scaling, y = breaks,
                label = labels,
                size = font_size,
                hjust = 0) +
        # scale_x_continuous(expand = c(expand_size,expand_size)) +
        scale_fill_manual(values=plotcolors) +
        theme_void() +
        expand_limits(y = c(ybottom_abs, ytop_abs), x = c(xend, x - 0.1 * mul * spacing_scaling))

    # Add legend title
    if (!is.null(legend_title)) {
        cbar_plot <- cbar_plot +
            annotate(
              geom = 'text',
              x = legend_position,
              # y = mean(r_breaks),
              y = max(r_breaks) + d_breaks * 1.5,
              label = legend_title,
              # angle = angle,
              angle = 0,
              size = font_size,
              fontface = 1,
              hjust = 0
              )
    }

    return(cbar_plot)
}

mycrop <- function(x, domain){

  # domain should be a vector of four values: c(xmin, xmax, ymin, ymax)
  x@data$id <- rownames(x@data)

  fortify(x, region="id") %>%
    as_tibble() %>%
    dplyr::left_join(x@data, by = "id") %>%
    dplyr::filter(long > domain[1] & long < domain[2] &
                    lat > domain[3] & lat < domain[4])
}
