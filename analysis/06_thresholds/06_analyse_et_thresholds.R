#!/usr/bin/env Rscript

# Analyse ET thresholds
# Extracted from vignettes/archive/workflow_legacy.Rmd.
source("analysis/_common.R")

load("data/df_cwd_et0_3.RData")
df_cwd_et0 <- df
rm("df")

## Detect flattening
df_cwd_et0 <- df_cwd_et0 |>
  mutate(flat_fet = ifelse(type_fet %in% c("Aa", "A1", "A2a"), TRUE, NA))

df_irr <- nc_to_df("~/data/irrigation/gmia_v5_aai_pct_0_05deg.nc", varnam = "aai") |> 
      drop_na() |> 
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) |> 
      rename(irr = aai)

save(df_irr, file = "data/df_irr.RData")
save(df_cwd_et0, file = "data/df_cwd_et0_3_flattening.RData")

df_cwd_et0 |> 
  ggplot(aes(x = cwd_lue0_fet, ..density..)) +
  geom_histogram() +
  xlim(0, 2500)

load("data/df_cwd_et0_3_flattening.RData")
load("data/df_rivers.RData")
load("data/df_irr.RData")
source("R/plot_map_cwdx_type_irrigation.R")

# ## apply vegetation mask
# load("data/df_vegmask.RData") # loads df_vegmask
# df_cwd_et0 <- df_cwd_et0 |> 
#   left_join(df_vegmask |> dplyr::select(lon, lat, vegmask),
#             by = c("lon", "lat")) |> 
#   mutate(cwd_lue0_fet = cwd_lue0_fet * vegmask)

## central asia, multi layer map
gg <- plot_map_cwdx_type_irrigation(
  df_cwd_et0, 
  df_irr,
  name_cwdx = "cwd_lue0_fet", name_type = "flat_fet", name_irr = "irr",
  lonmin = 45, lonmax = 95, latmin = 25, latmax = 47.5,
  # lonmin = 52, lonmax = 85, latmin = 27, latmax = 45,
  breaks = c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf), 
  spacing = "constant",
  combine = FALSE, 
  colorscale = "batlowK", 
  legend_title = "(mm)", 
  hillshade = TRUE,
  rivers = TRUE,
  ocean = FALSE,
  scale = "medium",
  expand_size_y = 0.7,
  irr_cutoff = 0.3
  )
gg$ggmap <- gg$ggmap +
  labs(title = expression(paste(italic("S")[dEF]))) +
  # mountains
  annotate(geom = "text", x = 50.5, y = 32, label = "M", fontface = 2, color = "green") +
  annotate(geom = "text", x = 79.5, y = 36, label = "M", fontface = 2, color = "green") +
  annotate(geom = "text", x = 65, y = 34, label = "M", fontface = 2, color = "green") +
  # delta
  annotate(geom = "text", x = 49, y = 45.5, label = "D", fontface = 2, color = "green") +
  # river
  annotate(geom = "text", x = 63.3, y = 31.1, label = "R", fontface = 2, color = "green") +
  annotate(geom = "text", x = 66, y = 43.5, label = "R", fontface = 2, color = "green")

gg <- cowplot::plot_grid(gg$ggmap, gg$gglegend, ncol = 2, rel_widths = c(1, 0.15))
gg
ggsave("fig/fig2.png", width = 10, height = 5)
ggsave("fig/fig2.pdf", width = 10, height = 5)

## save for publication figure
gg_fig1c <- gg
save(gg_fig1c, file = "data/gg_fig1c.Rdata")

## Western USA, multi layer map
gg <- plot_map_cwdx_type_irrigation(
  df_cwd_et0, df_irr,
  name_cwdx = "cwd_lue0_fet", name_type = "flat_fet", name_irr = "irr",
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
ggsave("fig/map_cwd_lue0_types_fet0_wusa.pdf", width = 10, height = 6)
ggsave("fig/map_cwd_lue0_types_fet0_wusa.png", width = 10, height = 6)


## Amazon, multi layer map
gg <- plot_map_cwdx_type_irrigation(
  df_cwd_et0, df_irr,
  name_cwdx = "cwd_lue0_fet", name_type = "flat_fet", name_irr = "irr",
  lonmin = -83, lonmax = -33, latmin = -25, latmax = 12,
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
ggsave("fig/map_cwd_lue0_types_fet0_amazon.pdf", width = 9, height = 6)
ggsave("fig/map_cwd_lue0_types_fet0_amazon.png", width = 9, height = 6)


## Europe, multi layer map
gg <- plot_map_cwdx_type_irrigation(
  df_cwd_et0, df_irr,
  name_cwdx = "cwd_lue0_fet", name_type = "flat_fet", name_irr = "irr",
  lonmin = -10, lonmax = 25, latmin = 35, latmax = 55, 
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
ggsave("fig/map_cwd_lue0_types_fet0_europe.pdf", width = 8, height = 6)
ggsave("fig/map_cwd_lue0_types_fet0_europe.png", width = 8, height = 6)

## fET (ET/Rn)
nc <- df_to_grid(df_cwd_et0, varnam = "cwd_lue0_fet", lonnam = "lon", latnam = "lat")
write_nc2(nc, varnams = "cwd_lue0_fet", lon = df_cwd_et0$lon |> unique() |> sort(), lat = df_cwd_et0$lat |> unique() |> sort(), path = "data/cwd_lue0_fet.nc", make_zdim = FALSE)

## s0_teuling_et
nc <- df_to_grid(df_cwd_et0 |> mutate(s0_teuling_fet = remove_outliers(s0_teuling_fet, coef = 10)), varnam = "s0_teuling_et", lonnam = "lon", latnam = "lat")
write_nc2(nc, varnams = "s0_teuling_fet", lon = df_cwd_et0$lon |> unique() |> sort(), lat = df_cwd_et0$lat |> unique() |> sort(), path = "data/s0_teuling_fet.nc", make_zdim = FALSE)

## write as netcdf
load("data/df_cwd_et0_3_flattening.RData")

nc <- df_to_grid(df_cwd_et0, varnam = "flat_fet", lonnam = "lon", latnam = "lat")
image(nc)
write_nc2(nc, 
          varnams = "flat_fet", 
          lon = df_cwd_et0$lon |> unique() |> sort() |> round(digits = 3), 
          lat = df_cwd_et0$lat |> unique() |> sort() |> round(digits = 3), 
          path = "data/flat_fet.nc", 
          make_zdim = FALSE
          )
raster("data/flat_fet.nc")

lon_orig <- df_cwd_et0$lon |> unique() |> sort() |> round(digits = 3)
lon <- seq(min(lon_orig), max(lon_orig), by = 0.05)

lat_orig <- df_cwd_et0$lat |> unique() |> sort() |> round(digits = 3)
lat <- seq(min(lat_orig), max(lat_orig), by = 0.05)

df_tmp <- expand.grid(lon, lat) |> 
  as_tibble() |> 
  setNames(c("lon", "lat")) |> 
  mutate(lon = as.factor(lon),
         lat = as.factor(lat)) |> 
  left_join(
    df_cwd_et0 |> 
      mutate(lon = round(lon, digits = 3),
             lat = round(lat, digits = 3)) |> 
      mutate(lon = as.factor(lon),
             lat = as.factor(lat)),
    by = c("lon", "lat")
  ) |> 
  mutate(lon = as.numeric(lon),
         lat = as.numeric(lat))

nc2 <- df_to_grid(df_tmp, varnam = "flat_fet", lonnam = "lon", latnam = "lat")
image(nc2)

write_nc2(nc2, 
          varnams = "flat_fet", 
          lon = df_tmp$lon |> unique() |> sort(), 
          lat = df_tmp$lat |> unique() |> sort(), 
          path = "data/flat_fet.nc", 
          make_zdim = FALSE
          )

## test
image(nc)

rasta <- raster::raster("data/flat_fet.nc")
library(rasterVis)
rasterVis::levelplot(rasta)

df_lc <- nc_to_df("~/data/landcover/modis_landcover__LPDAAC__v5.1__0.05deg__2010.nc", 
                    varnam = "landcover") |> 
  mutate(lon = round(lon, digits = 1) - 0.05/2, lat = round(lat, digits = 1) - 0.05/2) |> 
  mutate(savannah_w = ifelse(landcover %in% c(9, 10), TRUE, FALSE), 
         savannah   = ifelse(landcover == 9, TRUE, FALSE))

# load("data/df_gti.RData")

df_tmp <- df_cwd_et0 |> 
  dplyr::select(lon, lat, flat_fet) |> 
  mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) |> 
  left_join(df_irr |> 
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)), 
    by = c("lon", "lat")) |> 
  left_join(df_lc, by = c("lon", "lat")) |> 
  left_join(
    nc_to_df("~/data/gti_marthews/ga2_0_05deg_median.nc", varnam = "gti") |>
      drop_na() |>
      rename(gti = myvar) |>
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)),
    by = c("lon", "lat"))

plot_map3(df_tmp, varnam = "landcover")

n_fun <- function(x){
  return(data.frame(y = 520,
                    label = length(x)))
}

## only woody savannah
gg1 <- df_tmp |> 
  drop_na(savannah) |> 
  group_by(savannah) |> 
  summarise(n_flat = sum(flat_fet, na.rm = TRUE), f_flat = sum(flat_fet, na.rm = TRUE)/n(), count = n()) |> 
  drop_na() |> 
  ggplot(aes(savannah, f_flat)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = "Savannah", y = "Fraction flattening")

## all land cover types
gg1b <- df_tmp |> 
  drop_na(landcover) |> 
  group_by(landcover) |> 
  summarise(f_flat = sum(flat_fet, na.rm = TRUE)/n()) |> 
  drop_na() |> 
  ggplot(aes(landcover, f_flat)) +
  geom_bar(stat = "identity") +
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5, size = 3) +
  theme_classic() +
  labs(x = "Land cover", y = "Fraction flattening")

## by bins in irrigated area fraction
df_irrbin <- df_tmp |> 
  drop_na(irr) |> 
  mutate(irr_bin = cut(irr, breaks = seq(0, 1, by = 0.2))) |> 
  group_by(irr_bin) |> 
  summarise(f_flat = sum(flat_fet, na.rm = TRUE)/n())

save(df_irrbin, file = "data/df_irrbin.RData")

gg2 <- df_irrbin |> 
  drop_na() |> 
  ggplot(aes(irr_bin, f_flat)) +
  geom_bar(stat = "identity") +
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5, size = 3) +
  theme_classic() +
  labs(x = "Irrigated area fraction (bins)", y = "Fraction flattening")

## create publication figure
plot_grid(gg1, gg2, ncol = 2, labels = c('a', 'b'))
ggsave("fig/flattening_analysis.pdf", width = 8, height = 4)
ggsave("fig/flattening_analysis.png", width = 8, height = 4)

load("data/df_cwd_et0_3.RData")
df_cwd_et0 <- df
rm("df")

## add vegetation mask (fraction of non-vegetated)
load("data/df_vegmask.RData") # loads df_vegmask
df_cwd_et0 <- df_cwd_et0 |>
  left_join(df_vegmask |> dplyr::select(lon, lat, nonveg),
            by = c("lon", "lat"))

dlon <- 0.1
dlat <- 0.1
lon_breaks <- seq(from = floor(min(df_cwd_et0$lon)), to = ceiling(max(df_cwd_et0$lon)), by = dlon)
lat_breaks <- seq(from = floor(min(df_cwd_et0$lat)), to = ceiling(max(df_cwd_et0$lat)), by = dlat)

df_cwd_et0 <- df_cwd_et0 |>
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

df_cwd_et0_agg <- df_cwd_et0 |> 
  group_by(lon_mid, lat_mid) |> 
  summarise(cwd_lue0_fet = mean(cwd_lue0_fet, na.rm = TRUE),
            nonveg = mean(nonveg, na.rm = TRUE)) |> 
  rename(lon = lon_mid, lat = lat_mid) |> 
  mutate(cwd_lue0_fet = ifelse(is.nan(cwd_lue0_fet), NA, cwd_lue0_fet))

save(df_cwd_et0_agg, file = "data/df_cwd_et0_agg.RData")

load("data/df_cwd_et0_agg.RData")

df_box <- tibble(
  long = c(45, 95, 95, 45, 45), lat = c(47.5, 47.5, 25, 25, 47.5),
  # long = c(52, 85, 85, 52, 52), lat = c(45, 45, 27, 27, 45), 
  order = 1:5, group = rep(1, 5)
  )

## apply vegetation mask
df_cwd_et0_agg <- df_cwd_et0_agg |> 
  mutate(cwd_lue0_fet = ifelse(nonveg > 0.99, NA, cwd_lue0_fet)) |> 
  mutate(cwd_lue0_fet = ifelse(lat > 74, NA, cwd_lue0_fet))  # implausible values higher north
  
## EF
gg <- plot_map4(df_cwd_et0_agg, 
                varnam = "cwd_lue0_fet", 
                breaks = c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf), 
                latmin = -60, latmax = 80,
                spacing = "constant", 
                maxval = 6000, 
                combine = FALSE, 
                colorscale = "batlowK", 
                legend_title = NULL, # "(mm)",
                hillshade = FALSE,
                rivers = FALSE,
                expand_size_y = 0.5,
                ocean = TRUE,
                legend_direction = "horizontal")
gg$ggmap <- gg$ggmap + 
  labs(title = expression(paste(italic("S")[dEF]))) +
  ## add red box for zoom
  geom_path(aes(x = long, y = lat, group = group), data = df_box, size = 0.3, color = "red", )
  
cowplot::plot_grid(gg$ggmap, gg$gglegend, ncol = 2, rel_heights = c(1, 0.2))
ggsave("fig/map_cwd_lue0_fet.pdf", width = 10, height = 6)
ggsave("fig/map_cwd_lue0_fet.png", width = 10, height = 6)

gg_fig1a <- gg$ggmap
save(gg_fig1a, file = "data/gg_fig1a.Rdata")

gg_fig1_legend <- gg$gglegend
save(gg_fig1_legend, file = "data/gg_fig1_legend.Rdata")

df_corr <- read_nc_onefile("data/cwdx20_halfdeg.nc", varnam = "cwdx20") |> 
  nc_to_df(varnam = "cwdx20") |> 
  rename(cwdx20 = myvar) |> 
  left_join(
    read_nc_onefile("data/cwd_lue0_et_halfdeg.nc", varnam = "cwd_lue0_et") |> 
      nc_to_df(varnam = "cwd_lue0_et") |> 
      rename(cwd_lue0_et = myvar),
    by = c("lon", "lat")
  ) |> 
  left_join(
    read_nc_onefile("data/cwd_lue0_et_halfdeg.nc", varnam = "cwd_et0_fet") |> 
      nc_to_df(varnam = "cwd_et0_fet") |> 
      rename(cwd_et0_fet = myvar),
    by = c("lon", "lat")
  ) |> 
  left_join(
    read_nc_onefile("data/s0_teuling_et_halfdeg.nc", varnam = "s0_teuling_et") |> 
      nc_to_df(varnam = "cwd_et0_fet") |> 
      rename(cwd_et0_fet = myvar),
    by = c("lon", "lat")
  )

out <- df_corr |> 
  filter(cwdx20 < 1000 & cwd_lue0_et < 1000) |> # TEST XXX
  analyse_modobs2("cwdx20", "cwd_lue0_et", type = "heat")
out$gg

df_corr |> 
  filter(cwdx20 < 1000 & cwd_et0_fet < 1000) |> # TEST XXX
  analyse_modobs2("cwdx20", "cwd_et0_fet", type = "heat")

df_corr |> 
  filter(cwdx20 < 1000 & s0_teuling_et < 1000) |> # TEST XXX
  analyse_modobs2("cwdx20", "s0_teuling_et", type = "heat")

df_corr <- df_cwdx |> 
  left_join(
    df |> 
      dplyr::select(lon, lat, cwd_lue0_et, cwd_lue0_fet, s0_teuling_et),
    by = c("lon", "lat")
  )

out <- df_corr |> 
  mutate(cwd_lue0_et = remove_outliers(cwd_lue0_et, coef = 5)) |> 
  analyse_modobs2("cwdx20", "cwd_lue0_et", type = "hex", plot_linmod = FALSE)
out$gg +
  scale_x_continuous(expand = c(0,0), limits = c(0,100)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,100))

out <- df_corr |> 
  mutate(cwd_lue0_fet = remove_outliers(cwd_lue0_fet, coef = 5)) |> 
  analyse_modobs2("cwdx20", "cwd_lue0_fet", type = "hex", plot_linmod = FALSE)
out$gg +
  scale_x_continuous(expand = c(0,0), limits = c(0,100)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,100))

# out <- df_corr |> 
#   mutate(cwd_lue0_et = remove_outliers(cwd_lue0_et, coef = 5)) |> 
#   analyse_modobs2("s0_teuling_et", "cwd_lue0_et", type = "hex", plot_linmod = FALSE)
# out$gg +
#   scale_x_continuous(expand = c(0,0), limits = c(0,1000)) + 
#   scale_y_continuous(expand = c(0,0), limits = c(0,100))

tmp <- df_cwd_lue0 |> 
  dplyr::select(lon, lat, cwd_lue0_nSIF) |> 
  left_join(df_cwd_et0 |> 
              dplyr::select(lon, lat, cwd_lue0_fet), 
            by = c("lon", "lat"))

out <- tmp |> 
  analyse_modobs2("cwd_lue0_fet", "cwd_lue0_nSIF", type = "hex", plot_linmod = FALSE)
gg_corr <- out$gg +
  scale_x_continuous(expand = c(0,0), limits = c(0,1200)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,1200)) +
  labs(x = expression(italic(S)[dEF] ~ " (mm)"), 
       y = expression(italic(S)[dSIF] ~ " (mm)")) +
  scale_fill_gradientn(
        colours = colorRampPalette( c("gray65", "navy", "red", "yellow"))(5), 
        trans = "log", breaks = c(1, 10, 100, 1000, 10000))
gg_corr
ggsave("fig/corr_cwd_lue0_nSIF_EF.pdf", width = 6, height = 5)
ggsave("fig/corr_cwd_lue0_nSIF_EF.png", width = 6, height = 5)

load("data/df_cwdx_10_20_40.RData") # loads 'df', created by analysis/04_cwd_extremes/04_collect_return_levels.R
df_cwdx <- df |> 
  mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) 

## new version
load("data/df_cwd_lue0_2.RData")
df_cwd_lue0 <- df |> 
  mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) 
rm("df")

load("data/df_cwd_et0_3.RData")  # loads 'df'
df_cwd_et0 <- df |> 
  mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) 
rm("df")

## old version
# load("data/df_cwd_lue0_SIF.RData")
# load("data/df_cwd_lue0_nSIF.RData")
# load("data/df_cwd_lue0_et.RData")
# load("data/df_cwd_lue0_fet.RData")

nc_landcover <- read_nc_onefile("~/data/landcover/modis_landcover__LPDAAC__v5.1__0.05deg__2010.nc", varnam = "landcover")
df_biome <- read_csv("~/data/biomes/wwf_ecoregions/biome_id.csv") |> 
  rename(biome = biome_id)

df_corr <- df_cwdx |> 
  left_join(df_cwd_lue0, by = c("lon", "lat")) |> 
  left_join(df_cwd_et0, by = c("lon", "lat")) |> 
  left_join(
    nc_to_df("~/data/biomes/wwf_ecoregions/official/wwf_terr_ecos_raster.nc", varnam = "biome") |> 
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)),
    by = c("lon", "lat")
  ) |> 
  left_join(nc_to_df(nc_landcover, varnam = "landcover") |> 
              mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)), 
            by = c("lon", "lat")) |> 
  left_join(df_biome, by = "biome") |> 
  mutate(forest = biome %in% c(1,2,3,4,5,6,12),
         grassland = biome %in% c(7,8))

save(df_corr, file = "data/df_corr.RData")
