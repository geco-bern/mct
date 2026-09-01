#!/usr/bin/env Rscript

# Create CWDX products and maps
# Extracted from vignettes/archive/workflow_legacy.Rmd.
source("analysis/_common.R")

## CWDX data
load("data/df_cwdx_10_20_40.RData") # loads 'df', created by analysis/04_cwd_extremes/04_collect_return_levels.R

df_cwdx <- df |> 
  mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) 

rm("df")

## mask out non-land cells based on zroot (get info from soil data)
load("data/df_mask.RData")
df_cwdx <- df_cwdx |> 
  left_join(df_mask, by = c("lon", "lat"))
for (i in 3:8){
  df_cwdx[,i] <- df_cwdx[,i] * df_cwdx[,9]
}

df_cwdx |> 
  ggplot(aes(x = cwdx80, ..density..)) +
  geom_histogram() +
  xlim(0, 2500)

gt <- df_cwdx |> 
  filter(cwdx80 > 300) |> 
  pull(cwdx80) |> 
  length()

all <- df_cwdx |> 
  filter(!is.na(cwdx80)) |> 
  pull(cwdx80) |> 
  length()

gt/all

## 10 y return period
nc <- df_to_grid(df_cwdx, varnam = "cwdx10", lonnam = "lon", latnam = "lat")
write_nc2(nc, varnams = "cwdx10", lon = df_cwdx$lon |> unique() |> sort(), lat = df_cwdx$lat |> unique() |> sort(), path = "data/cwdx10.nc", make_zdim = FALSE)

## 20 y return period
nc <- df_to_grid(df_cwdx, varnam = "cwdx20", lonnam = "lon", latnam = "lat")
write_nc2(nc, varnams = "cwdx20", lon = df_cwdx$lon |> unique() |> sort(), lat = df_cwdx$lat |> unique() |> sort(), path = "data/cwdx20.nc", make_zdim = FALSE)

## 40 y return period
nc <- df_to_grid(df_cwdx, varnam = "cwdx40", lonnam = "lon", latnam = "lat")
write_nc2(nc, varnams = "cwdx40", lon = df_cwdx$lon |> unique() |> sort(), lat = df_cwdx$lat |> unique() |> sort(), path = "data/cwdx40.nc", make_zdim = FALSE)

## 80 y return period
nc <- df_to_grid(df_cwdx, varnam = "cwdx80", lonnam = "lon", latnam = "lat")
write_nc2(nc, varnams = "cwdx80", lon = df_cwdx$lon |> unique() |> sort(), lat = df_cwdx$lat |> unique() |> sort(), path = "data/cwdx80.nc", make_zdim = FALSE)

## 100 y return period
nc <- df_to_grid(df_cwdx, varnam = "cwdx100", lonnam = "lon", latnam = "lat")
write_nc2(nc, varnams = "cwdx100", lon = df_cwdx$lon |> unique() |> sort(), lat = df_cwdx$lat |> unique() |> sort(), path = "data/cwdx100.nc", make_zdim = FALSE)

## 200 y return period
nc <- df_to_grid(df_cwdx, varnam = "cwdx200", lonnam = "lon", latnam = "lat")
write_nc2(nc, varnams = "cwdx200", lon = df_cwdx$lon |> unique() |> sort(), lat = df_cwdx$lat |> unique() |> sort(), path = "data/cwdx200.nc", make_zdim = FALSE)

system("cdo remapbil,data-raw/grid/gridfile_tenthdeg.txt data/cwdx10.nc data/cwdx10_tenthdeg.nc")
system("cdo remapbil,data-raw/grid/gridfile_tenthdeg.txt data/cwdx20.nc data/cwdx20_tenthdeg.nc")
system("cdo remapbil,data-raw/grid/gridfile_tenthdeg.txt data/cwdx40.nc data/cwdx40_tenthdeg.nc")
system("cdo remapbil,data-raw/grid/gridfile_tenthdeg.txt data/cwdx80.nc data/cwdx80_tenthdeg.nc")
system("cdo remapbil,data-raw/grid/gridfile_tenthdeg.txt data/cwdx100.nc data/cwdx100_tenthdeg.nc")
system("cdo remapbil,data-raw/grid/gridfile_tenthdeg.txt data/cwdx200.nc data/cwdx200_tenthdeg.nc")

source("R/extract_cwdx_byilon_lores.R")
out <- purrr::map_dfr(as.list(seq(720)), ~extract_cwdx_byilon_lores(.))

load("data/df_vegmask_tenthdeg.RData")  # loads df_vegmask_tenthdeg

## 80 yr
nc_tenthdeg <- nc_to_df("data/cwdx80_tenthdeg.nc", varnam = "cwdx80") |> 
  mutate(lon = round(lon, digits = 2), lat = round(lat, digits = 2)) |>
  left_join(df_vegmask_tenthdeg |>
              mutate(lon = round(lon, digits = 2), lat = round(lat, digits = 2)),
            by = c("lon", "lat")) |>
  mutate(cwdx80 = ifelse(nonveg > 99, NA, cwdx80)) |> 
  mutate(cwdx80 = ifelse(lat > 74, NA, cwdx80))

gg <- plot_map4(nc_tenthdeg, 
                varnam = "cwdx80", 
                breaks = c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf), 
                latmin = -60, latmax = 80,
                spacing = "constant", 
                maxval = 6000, 
                combine = FALSE, 
                colorscale = "batlowK", 
                legend_title = "(mm)",
                hillshade = FALSE,
                rivers = FALSE,
                ocean = TRUE,
                expand_size_y = 0.5,
                legend_direction = "vertical"
                )
gg$ggmap <- gg$ggmap + 
  labs(title = expression(paste(italic("S")[CWDX80])))

cowplot::plot_grid(gg$ggmap, gg$gglegend, ncol = 2, rel_widths = c(1, 0.12))
ggsave("fig/map_cwdx80.pdf", width = 10, height = 5)
ggsave("fig/map_cwdx80.png", width = 10, height = 5)

gg_fig3a <- gg$ggmap
gg_fig3a_legend <- gg$gglegend

# ## add rivers shapefile following https://www.earthdatascience.org/courses/earth-analytics/spatial-data-r/make-maps-with-ggplot-in-R/
# ## read rivers database
# rivers <- readOGR("~/data/world_rivers_dSe/world_rivers_dSe.shp")
# df_rivers <- tidy(rivers, region = "id")
# 
# # make sure the shapefile attribute table has an id column
# rivers$id <- rownames(rivers@data)
# 
# # join the attribute table from the spatial object to the new data frame
# df_rivers <- left_join(df_rivers,
#                        rivers@data,
#                        by = "id"
#                        )
# save(df_rivers, file = "data/df_rivers.RData")

load("data/df_cwdx_10_20_40.RData") # loads 'df', created by analysis/04_cwd_extremes/04_collect_return_levels.R

df_cwdx <- df |> 
  mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) 

## apply vegetation mask
load("data/df_vegmask.RData") # loads df_vegmask
df_cwdx <- df_cwdx |>
  left_join(df_vegmask |> dplyr::select(lon, lat, vegmask),
            by = c("lon", "lat")) |>
  mutate(cwdx80 = cwdx80 * vegmask)

# ## spain
# gg <- df_cwdx |> 
#   dplyr::filter(lon > -10 & lon < 5 & lat > 35 & lat < 45) |> 
#   plot_map4(varnam = "cwdx80", lonmin = -10, lonmax = 5, latmin = 35, latmax = 45, 
#             breaks = c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf), 
#             spacing = "constant",
#             colorscale = "batlowK")
# 
# ## italy
# gg <- df_cwdx |> 
#   dplyr::filter(lon > 7.5 & lon < 20 & lat > 36 & lat < 47) |> 
#   plot_map3(varnam = "cwdx80", lonmin = 7.5, lonmax = 20, latmin = 36, latmax = 47, breaks = c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf), spacing = "constant")

## europe
gg <- df_cwdx |> 
  dplyr::filter(lon > -10 & lon < 25 & lat > 35 & lat < 55) |> 
  plot_map4(varnam = "cwdx80", 
            lonmin = -10, lonmax = 25, latmin = 35, latmax = 55, 
            breaks = c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf), 
            spacing = "constant", 
            colorscale = "batlowK",
            combine = FALSE, legend_title = "(mm)",
            expand_size_y = 0.5,
            hillshade = TRUE, rivers = TRUE, lakes = TRUE,
            scale = 50
            )
gg$ggmap <- gg$ggmap + 
  labs(title = expression(italic(S)[CWDX80]))
cowplot::plot_grid(gg$ggmap, gg$gglegend, ncol = 2, rel_widths = c(1, 0.1))
ggsave("fig/map_cwdx80_europe.pdf", width = 8, height = 6)
ggsave("fig/map_cwdx80_europe.png", width = 8, height = 6)

## western usa
gg <- df_cwdx |> 
  plot_map4(varnam = "cwdx80", 
            lonmin = -125, lonmax = -95, latmin = 32.5, latmax = 47, 
            breaks = c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf), 
            spacing = "constant", 
            colorscale = "batlowK",
            combine = FALSE, legend_title = "(mm)",
            expand_size_y = 0.5,
            hillshade = TRUE, rivers = TRUE, lakes = TRUE,
            scale = 50
            )
gg$ggmap <- gg$ggmap + 
  labs(title = expression(italic(S)[CWDX80]))
cowplot::plot_grid(gg$ggmap, gg$gglegend, ncol = 2, rel_widths = c(1, 0.1))
ggsave("fig/map_cwdx80_wusa.pdf", width = 10, height = 6)
ggsave("fig/map_cwdx80_wusa.png", width = 10, height = 6)

## central asia
gg <- df_cwdx |> 
  plot_map4(varnam = "cwdx80", lonmin = 45, lonmax = 95, latmin = 25, latmax = 47.5, 
            breaks = c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf), 
            spacing = "constant",
            combine = FALSE, 
            colorscale = "batlowK", legend_title = "(mm)",
            expand_size_y = 0.5,
            hillshade = TRUE, rivers = TRUE, lakes = TRUE,
            scale = 50
            )
gg$ggmap <- gg$ggmap + 
  labs(title = expression(italic(S)[CWDX80]))
cowplot::plot_grid(gg$ggmap, gg$gglegend, ncol = 2, rel_widths = c(1, 0.1))
ggsave("fig/map_cwdx80_casia.pdf", width = 9, height = 5)
ggsave("fig/map_cwdx80_casia.png", width = 9, height = 5)

## Amazon
gg <- df_cwdx |> 
  drop_na(cwdx80) |> 
  plot_map4(varnam = "cwdx80", lonmin = -83, lonmax = -33, latmin = -25, latmax = 12, 
            breaks = c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf), 
            spacing = "constant",
            combine = FALSE, 
            colorscale = "batlowK", legend_title = "(mm)",
            hillshade = TRUE, rivers = TRUE, lakes = TRUE, ocean = TRUE,
            expand_size_y = 0.6,
            scale = 50
            )
gg$ggmap <- gg$ggmap + 
  labs(title = expression(italic(S)[CWDX80])) 
cowplot::plot_grid(gg$ggmap, gg$gglegend, ncol = 2, rel_widths = c(1, 0.1))
ggsave("fig/map_cwdx80_amazon.pdf", width = 9, height = 6)
ggsave("fig/map_cwdx80_amazon.png", width = 9, height = 6)

load("data/df_cwdx_10_20_40.RData") # loads 'df', created by analysis/04_cwd_extremes/04_collect_return_levels.R
load("data/df_vegmask.RData") # loads df_vegmask
df_cwdx <- df |> 
  mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) |> 
  left_join(df_vegmask |> dplyr::select(lon, lat, vegmask),
            by = c("lon", "lat")) |>
  mutate(cwdx80 = cwdx80 * vegmask)

gg_hist_cwdx80 <- df_cwdx |> 
  filter(cwdx80 < 1200) |> 
  ggplot(aes(x = cwdx80)) +
  # geom_density()
  geom_histogram(binwidth = 20) +
  xlim(0, 1200) +
  theme_classic() +
  labs(x = expression(italic(S)[CWDX80] ~ "(mm)"), y = "Count")

load("data/df_whc.RData")

library(patchwork)

df_whc <- df_whc |> 
  mutate(whc_1m = 300 * whc_top + 700 * whc_sub) |> 
  mutate(whc_2m = 300 * whc_top + 700 * whc_sub + 1000 * whc_sub)

gg1 <- df_whc |> 
  ggplot(aes(x = whc_1m, y = ..density..)) + 
  geom_histogram() + 
  xlim(0, 250) +
  labs(x = "WHC (mm)", subtitle = "Top 1 m soil")


gg2 <- df_whc |> 
  ggplot(aes(x = whc_2m, y = ..density..)) + 
  geom_histogram() + 
  xlim(0, 400) +
  labs(x = "WHC (mm)", subtitle = "Top 2 m soil")

gg1 + gg2

ggsave("fig/whc_soil.pdf", width = 8, height = 4)
ggsave("fig/whc_soil.png", width = 8, height = 4)

nc <- df_to_grid(df_whc, 
                 varnam = "whc_1m", 
                 lonnam = "lon", 
                 latnam = "lat"
                 )
write_nc2(nc, 
          varnams = "whc_1m", 
          lon = df_whc$lon |> unique() |> sort(), 
          lat = df_whc$lat |> unique() |> sort(), 
          path = "data/whc_1m.nc", 
          make_zdim = FALSE
          )

nc <- df_to_grid(df_whc, 
                 varnam = "whc_2m", 
                 lonnam = "lon", 
                 latnam = "lat"
                 )
write_nc2(nc, 
          varnams = "whc_2m", 
          lon = df_whc$lon |> unique() |> sort(), 
          lat = df_whc$lat |> unique() |> sort(), 
          path = "data/whc_2m.nc", 
          make_zdim = FALSE
          )

# source("R/regrid_df.R")
# df_whc_agg <- regrid_df(df_whc, varnam = "whc_1m", dlon = 0.1, dlat = 0.1)

lon_breaks <- seq(from = floor(min(df_whc$lon)), to = ceiling(max(df_whc$lon)), by = 0.1)
lat_breaks <- seq(from = floor(min(df_whc$lat)), to = ceiling(max(df_whc$lat)), by = 0.1)

df_whc <- df_whc |>
  ungroup() |>
  mutate(ilon = cut(lon,
                    breaks = lon_breaks),
         ilat = cut(lat,
                    breaks = lat_breaks)) |>
  mutate(lon_lower = as.numeric( sub("\\((.+),.*", "\\1", ilon)),
         lon_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilon) ),
         lat_lower = as.numeric( sub("\\((.+),.*", "\\1", ilat) ),
         lat_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilat) )) |>
  mutate(lon_mid = (lon_lower + lon_upper)/2,
         lat_mid = (lat_lower + lat_upper)/2) |>
  
  ## create cell name to associate with climate input
  dplyr::select(-ilon, -ilat, -lon_lower, -lon_upper, -lat_lower, -lat_upper)

df_whc_agg <- df_whc |> 
  group_by(lon_mid, lat_mid) |> 
  summarise(whc_1m = mean(whc_1m, na.rm = TRUE), whc_2m = mean(whc_2m, na.rm = TRUE)) |> 
  rename(lon = lon_mid, lat = lat_mid)
  # mutate(!!varnam := ifelse(is.nan(!!varnam), NA, !!varnam))

saveRDS(
  df_whc_agg,
  file = here::here("data/df_whc_agg.rds")
)

gg1 <- plot_map4(df_whc_agg, 
                varnam = "whc_1m", 
                breaks = c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf), 
                latmin = -60, latmax = 80,
                spacing = "constant", 
                maxval = 6000, 
                combine = TRUE, 
                colorscale = "batlowK", 
                legend_title = "(mm)",
                expand_size_y = 0.5,
                ocean = TRUE)

# ggsave("fig/map_whc_1m.pdf", width = 10, height = 5)
# ggsave("fig/map_whc_1m.png", width = 10, height = 5)

gg2 <- plot_map4(df_whc_agg, 
                varnam = "whc_2m", 
                breaks = c(seq(0, 100, by = 20), 150, 200, 300, 500, 700, 900, 1200, Inf), 
                latmin = -60, latmax = 80,
                spacing = "constant", 
                maxval = 6000, 
                combine = TRUE, 
                colorscale = "batlowK", 
                legend_title = "(mm)",
                expand_size_y = 0.5,
                ocean = TRUE)

# ggsave("fig/map_whc_2m.pdf", width = 10, height = 5)
# ggsave("fig/map_whc_2m.png", width = 10, height = 5)

plot_grid(gg1, gg2, ncol = 1, labels = c('a', 'b'))
ggsave("fig/map_whc_1m_2m.pdf", width = 10, height = 9)
ggsave("fig/map_whc_1m_2m.png", width = 10, height = 9)

load("data/df_cwdx_10_20_40.RData") # loads 'df', created by analysis/04_cwd_extremes/04_collect_return_levels.R
df_cwdx <- df |> 
  mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) 
rm("df")
load("data/df_vegmask.RData") # loads df_vegmask

df <- df_whc |> 
  dplyr::select(lon, lat, whc_1m, whc_2m) |> 
  left_join(
    df_cwdx,
    by = c("lon", "lat")
  ) |>
  left_join(df_vegmask |> 
              dplyr::select(lon, lat, nonveg),
            by = c("lon", "lat")) |> 
  mutate(area = rbeni::calc_area(lat, dx = 0.05, dy = 0.05)) |> 
  mutate(deeper_2m = ifelse(cwdx80 > whc_2m, 1, 0),
         deeper_1m = ifelse(cwdx80 > whc_1m, 1, 0)) |> 
  mutate(vegetated = ifelse(nonveg < 0.99, 1, 0))

## fraction of vegetated earth surface where plants have access to deep (>2 m) water
sum_deeper_1m <- df |> 
  mutate(tmp = area * vegetated * deeper_1m) |> 
  summarise(tmp = sum(tmp, na.rm = TRUE)) |> 
  pull(tmp)
sum_deeper_2m <- df |> 
  mutate(tmp = area * vegetated * deeper_2m) |> 
  summarise(tmp = sum(tmp, na.rm = TRUE)) |> 
  pull(tmp)
sum_vegetated <- df |> 
    mutate(tmp = area * vegetated) |> 
    summarise(tmp = sum(tmp, na.rm = TRUE)) |> 
    pull(tmp)
sum_deeper_1m / sum_vegetated
sum_deeper_2m / sum_vegetated

df <- df |>
  ungroup() |>
  mutate(ilon = cut(lon,
                    breaks = lon_breaks),
         ilat = cut(lat,
                    breaks = lat_breaks)) |>
  mutate(lon_lower = as.numeric( sub("\\((.+),.*", "\\1", ilon)),
         lon_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilon) ),
         lat_lower = as.numeric( sub("\\((.+),.*", "\\1", ilat) ),
         lat_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilat) )) |>
  mutate(lon_mid = (lon_lower + lon_upper)/2,
         lat_mid = (lat_lower + lat_upper)/2) |>
  
  ## create cell name to associate with climate input
  dplyr::select(-ilon, -ilat, -lon_lower, -lon_upper, -lat_lower, -lat_upper)

df_agg <- df |> 
  group_by(lon_mid, lat_mid) |> 
  summarise(deeper_1m = mean(deeper_1m, na.rm = TRUE), 
            deeper_2m = mean(deeper_2m, na.rm = TRUE)) |> 
  mutate(deeper_1m = ifelse(deeper_1m < 0.5, FALSE, TRUE), 
         deeper_2m = ifelse(deeper_2m < 0.5, FALSE, TRUE)) |> 
  rename(lon = lon_mid, lat = lat_mid)

gg3 <- plot_map4(df_agg, 
                varnam = "deeper_1m", 
                # breaks = c(TRUE, FALSE), 
                latmin = -60, latmax = 80,
                # spacing = "constant", 
                # maxval = 6000, 
                # combine = TRUE, 
                # colorscale = viridis::cividis, 
                # legend_title = "",
                # expand_size_y = 0.5,
                ocean = TRUE,
                is_boolean = TRUE
                )

# ggsave("fig/map_whc_1m.pdf", width = 10, height = 5)
# ggsave("fig/map_whc_1m.png", width = 10, height = 5)

gg4 <- plot_map4(df_agg, 
                varnam = "deeper_2m", 
                # breaks = c(TRUE, FALSE), 
                latmin = -60, latmax = 80,
                # spacing = "constant", 
                # maxval = 6000, 
                # combine = TRUE, 
                # colorscale = viridis::cividis, 
                # legend_title = "",
                # expand_size_y = 0.5,
                ocean = TRUE,
                is_boolean = TRUE
                )

# ggsave("fig/map_whc_2m.pdf", width = 10, height = 5)
# ggsave("fig/map_whc_2m.png", width = 10, height = 5)

plot_grid(gg3$ggmap, gg4$ggmap, ncol = 1, labels = c('a', 'b'))
ggsave("fig/map_whc_1m_2m_deeper.png", width = 10, height = 9)
ggsave("fig/map_whc_1m_2m_deeper.pdf", width = 10, height = 9)
