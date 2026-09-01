#!/usr/bin/env Rscript

# Analyse SIF thresholds
# Extracted from vignettes/archive/workflow_legacy.Rmd.
source("analysis/_common.R")

load("data/df_cwd_lue0_2.RData")
df_cwd_lue0 <- df
rm("df")

load("data/df_rivers.RData")
source("R/plot_map_cwdx_type_irrigation.R")

## Detect flattening
df_cwd_lue0 <- df_cwd_lue0 |>
  mutate(flat_nsif = ifelse(type_nSIF %in% c("Aa", "A1", "A2a"), TRUE, NA))

df_irr <- nc_to_df("~/data/irrigation/gmia_v5_aai_pct_0_05deg.nc", varnam = "aai") |> 
      drop_na() |> 
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) |> 
      rename(irr = aai)

## central asia SIF
gg <- df_cwd_lue0 |>
  plot_map4(varnam = "cwd_lue0_nSIF", lonmin = 45, lonmax = 95, latmin = 25, latmax = 47.5,
            breaks = c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf),
            spacing = "constant",
            combine = FALSE,
            colorscale = "batlowK",
            legend_title = "(mm)",
            hillshade = TRUE,
            rivers = TRUE,
            scale = 50
            )
gg$ggmap <- gg$ggmap +
  labs(title = expression(paste(italic("S")[dEF])))
cowplot::plot_grid(gg$ggmap, gg$gglegend, ncol = 2, rel_widths = c(1, 0.2))
ggsave("fig/map_cwd_lue0_fet0_casia2.pdf", width = 10, height = 6)


# ## central asia S0-Teuling
# gg <- df_cwd_lue0 |> 
#   plot_map4(varnam = "s0_teuling_fet", lonmin = 45, lonmax = 95, latmin = 25, latmax = 47.5, 
#             breaks = c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf), 
#             spacing = "constant",
#             combine = FALSE, 
#             colorscale = "batlowK"
#             )
# gg$ggmap <- gg$ggmap + 
#   labs(title = expression(paste(italic("S")[0-Teuling]))) +
#   geom_path(data = df_rivers, aes(x = long, y = lat, group = group), color = "dodgerblue")
# cowplot::plot_grid(gg$ggmap, gg$gglegend, ncol = 2, rel_widths = c(1, 0.2))
# ggsave("fig/map_s0_teuling_fet_casia2.pdf", width = 10, height = 6)
# ggsave("fig/map_s0_teuling_fet_casia2.png", width = 10, height = 6)

## Western USA, multi layer map
gg <- plot_map_cwdx_type_irrigation(
  df_cwd_lue0, df_irr,
  name_cwdx = "cwd_lue0_nSIF", name_type = "flat_nsif", name_irr = "irr",
  lonmin = -125, lonmax = -95, latmin = 32.5, latmax = 47, 
  breaks = c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf), 
  spacing = "constant",
  combine = FALSE, 
  colorscale = "batlowK", 
  legend_title = expression(paste(italic("S")[dEF], " (mm)")), 
  hillshade = TRUE,
  rivers = TRUE,
  scale = "medium",
  expand_size_y = 0.4,
  irr_cutoff = 0.3
  )
cowplot::plot_grid(gg$ggmap, gg$gglegend, ncol = 2, rel_widths = c(1, 0.15))
ggsave("fig/map_cwd_lue0_types_lue0_wusa.pdf", width = 10, height = 6)
ggsave("fig/map_cwd_lue0_types_lue0_wusa.png", width = 10, height = 6)


## Amazon, multi layer map
gg <- plot_map_cwdx_type_irrigation(
  df_cwd_lue0, df_irr,
  name_cwdx = "cwd_lue0_nSIF", name_type = "flat_nsif", name_irr = "irr",
  lonmin = -83, lonmax = -33, latmin = -25, latmax = 12,
  breaks = c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf), 
  spacing = "constant",
  combine = FALSE, 
  colorscale = "batlowK", 
  legend_title = expression(paste(italic("S")[dEF], " (mm)")), 
  hillshade = TRUE,
  rivers = TRUE,
  scale = 50,
  expand_size_y = 0.4,
  irr_cutoff = 0.3
  )
cowplot::plot_grid(gg$ggmap, gg$gglegend, ncol = 2, rel_widths = c(1, 0.15))
ggsave("fig/map_cwd_lue0_types_lue0_amazon.pdf", width = 9, height = 6)
ggsave("fig/map_cwd_lue0_types_lue0_amazon.png", width = 9, height = 6)


## Europe, multi layer map
gg <- plot_map_cwdx_type_irrigation(
  df_cwd_lue0, df_irr,
  name_cwdx = "cwd_lue0_nSIF", name_type = "flat_nsif", name_irr = "irr",
  lonmin = -10, lonmax = 25, latmin = 35, latmax = 55, 
  breaks = c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf), 
  spacing = "constant",
  combine = FALSE, 
  colorscale = "batlowK", 
  legend_title = expression(paste(italic("S")[dEF], " (mm)")), 
  hillshade = TRUE,
  rivers = TRUE,
  scale = 50,
  expand_size_y = 0.4,
  irr_cutoff = 0.3,
  legend_title = "(mm)"
  )
cowplot::plot_grid(gg$ggmap, gg$gglegend, ncol = 2, rel_widths = c(1, 0.15))
ggsave("fig/map_cwd_lue0_types_lue0_europe.pdf", width = 8, height = 6)
ggsave("fig/map_cwd_lue0_types_lue0_europe.png", width = 8, height = 6)

## add vegetation mask (fraction of non-vegetated)
# load("data/df_vegmask.RData") # loads df_vegmask
df_cwd_lue0 <- df_cwd_lue0 |>
  left_join(df_vegmask |> dplyr::select(lon, lat, nonveg),
            by = c("lon", "lat"))

## bin to half-degree gridcells for determining climate forcing
dlon <- 0.1
dlat <- 0.1
lon_breaks <- seq(from = floor(min(df_cwd_lue0$lon)), to = ceiling(max(df_cwd_lue0$lon)), by = dlon)
lat_breaks <- seq(from = floor(min(df_cwd_lue0$lat)), to = ceiling(max(df_cwd_lue0$lat)), by = dlat)

df_cwd_lue0 <- df_cwd_lue0 |>
  ungroup() |>
  mutate(ilon = cut(lon,
                    breaks = lon_breaks
                    ),
         ilat = cut(lat,
                    breaks = lat_breaks
                    )
         ) |>
  mutate(lon_lower = as.numeric( sub("\\((.+),.*", "\\1", ilon)),
         lon_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilon) ),
         lat_lower = as.numeric( sub("\\((.+),.*", "\\1", ilat) ),
         lat_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilat) )
         ) |>
  mutate(lon_mid = (lon_lower + lon_upper)/2,
         lat_mid = (lat_lower + lat_upper)/2) |>

  ## create cell name to associate with climate input
  dplyr::select(-ilon, -ilat, -lon_lower, -lon_upper, -lat_lower, -lat_upper)

df_cwd_lue0_agg <- df_cwd_lue0 |> 
  group_by(lon_mid, lat_mid) |> 
  summarise(cwd_lue0_SIF = mean(cwd_lue0_SIF, na.rm = TRUE), 
            cwd_lue0_nSIF = mean(cwd_lue0_nSIF, na.rm = TRUE),
            nonveg = mean(nonveg, na.rm = TRUE)) |> 
  rename(lon = lon_mid, lat = lat_mid) |> 
  mutate(cwd_lue0_SIF = ifelse(is.nan(cwd_lue0_SIF), NA, cwd_lue0_SIF),
         cwd_lue0_nSIF = ifelse(is.nan(cwd_lue0_nSIF), NA, cwd_lue0_nSIF))

save(df_cwd_lue0_agg, file = "data/df_cwd_lue0_agg.Rdata")

load("data/df_cwd_lue0_agg.Rdata")

# apply vegetation mask
df_cwd_lue0_agg <- df_cwd_lue0_agg |> 
  # mutate(cwd_lue0_nSIF = ifelse(nonveg > 0.99, NA, cwd_lue0_nSIF)) |> 
  mutate(cwd_lue0_nSIF = ifelse(lat > 74, NA, cwd_lue0_nSIF))  # values higher north are implausible

## nSIF
# nc_tenthdeg <- read_nc_onefile("data/cwd_lue0_nSIF_tenthdeg.nc", varnam = "cwd_lue0_nSIF")
gg <- plot_map4(df_cwd_lue0_agg, 
                varnam = "cwd_lue0_nSIF", 
                breaks = c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf), 
                latmin = -60, latmax = 80,
                spacing = "constant", 
                maxval = 6000, 
                combine = FALSE, 
                colorscale = "batlowK", 
                legend_title = "(mm)",
                legend_direction = "horizontal",
                ocean = TRUE
                )
gg$ggmap <- gg$ggmap + 
  labs(title = expression(paste(italic("S")[dSIF])))
cowplot::plot_grid(gg$ggmap, gg$gglegend, ncol = 1, rel_heights = c(1, 0.2))
ggsave("fig/map_cwd_lue0_nSIF.pdf", width = 10, height = 6)
ggsave("fig/map_cwd_lue0_nSIF.png", width = 10, height = 6)

gg_fig1b <- gg$ggmap
gg_fig1_legend <- gg$gglegend
save(gg_fig1b, file = "data/gg_fig1b.Rdata")

# ## lambda_decay_SIF
# nc_tenthdeg <- read_nc_onefile("data/lambda_decay_SIF_tenthdeg.nc", varnam = "lambda_decay_SIF")
# plot_map3(nc_tenthdeg, varnam = "lambda_decay_SIF", breaks = c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf), latmin = -60, latmax = 80,
#           spacing = "constant", maxval = 6000)
# ggsave("fig/map_lambda_decay_SIF.pdf", width = 10, height = 6)
# 
# ## lambda_decay_nSIF
# nc_tenthdeg <- read_nc_onefile("data/lambda_decay_nSIF_tenthdeg.nc", varnam = "lambda_decay_nSIF")
# plot_map3(nc_tenthdeg, varnam = "lambda_decay_nSIF", breaks = -(c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf)), latmin = -60, latmax = 80,
#           spacing = "constant", maxval = 6000)
# ggsave("fig/map_lambda_decay_nSIF.pdf", width = 10, height = 6)

## all longitudes
lon_all <- seq(from = -179.975, to = 179.975, by = 0.05) |> 
  round(digits = 3)

## available lons
lon_avl <- df_cwd_lue0 |> 
  filter(!is.na(cwd_lue0_nSIF)) |> 
  pull(lon) |> 
  unique() |> 
  round(digits = 3)

## missing lons
lon_missing <- lon_all[!(lon_all %in% lon_avl)]
ilon_missing <- (lon_missing + 179.975)/0.05 + 1

saveRDS(ilon_missing, file = "data/ilon_missing.rds")

## relevant: 3961-4176 and 2161-2232

plot(lon_all, !(lon_all %in% lon_avl), type = "l")
plot(!(lon_all %in% lon_avl), type = "l")

