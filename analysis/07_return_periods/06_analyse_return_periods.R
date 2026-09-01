#!/usr/bin/env Rscript

# Analyse diagnosed return periods
# Extracted from vignettes/archive/workflow_legacy.Rmd.
source("analysis/_common.R")

load("data/df_rl_fet.RData")  # loads df_rl_fet

## function to determine the return period with the lowest bias (absolute)
find_rp_diag <- function(df){
  out <- df |> 
    group_by(return_period) |> 
    summarise(logbias = median(logbias, na.rm = TRUE)) |> 
    slice(which.min(abs(logbias))) |> 
    pull(return_period)
  out <- ifelse(length(out)==0, NA, out)
  return(out)
}

## bin into gridcells to be able to get a distribution (0.5 deg contains a maximum of 100 pixels)
dlon <- 1
dlat <- 1
lon_breaks <- seq(from = -180, to = 180, by = dlon)
lat_breaks <- seq(from = -90, to = 90, by = dlat)

## this takes about 1 min
df_rl_agg_fet <- df_rl_fet |>
  ungroup() |> 
  mutate(ilon = cut(lon, 
                    breaks = lon_breaks
                    # labels = as.character(seq(length(lon_breaks)-1))
                    ),
         ilat = cut(lat, 
                    breaks = lat_breaks
                    # labels = as.character(seq(length(lat_breaks)-1))
                    )
         ) |> 
  mutate(lon_lower = as.numeric( sub("\\((.+),.*", "\\1", ilon)),
         lon_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilon) ),
         lat_lower = as.numeric( sub("\\((.+),.*", "\\1", ilat) ),
         lat_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilat) )
         ) |> 
  mutate(lon_mid = (lon_lower + lon_upper)/2,
         lat_mid = (lat_lower + lat_upper)/2) |> 
  dplyr::select(-ilon, -ilat, -lon_lower, -lat_lower, -lat_upper, -lon_upper) |> 
  
  ## nest by gridcell and determine return period for which the absolute of the bias is lowest
  group_by(lon_mid, lat_mid) |> 
  nest() |> 
  ungroup() |> 
  mutate(rp_diag = purrr::map_dbl(data, ~find_rp_diag(.))) |> 
  dplyr::select(-data) |> 
  rename(lon = lon_mid, lat = lat_mid)

save(df_rl_agg_fet, file = "data/df_rl_agg_fet.RData")

## distribution of diagnosed return periods
df_rl_agg_fet |> 
  ggplot(aes(rp_diag, ..count..)) + 
  geom_histogram()

## global analysis of best return period
df_rl_fet |> 
  ggplot(aes(as.factor(return_period), logbias)) +
  # geom_violin(draw_quantiles = 0.5) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  ylim(-1.2, 1.2)

save(df_rl_fet, file = "data/df_rl_fet.RData")

load("data/df_rl_agg_fet.RData")

# nc <- df_to_grid(
#   df_rl_agg, 
#   varnam = "rp_diag", 
#   lonnam = "lon", latnam = "lat"
#   )

df_rl_agg_fet <- df_rl_agg_fet |> 
  mutate(rp_diag = ifelse(lat > 75, NA, rp_diag))

gg <- plot_map4(df_rl_agg_fet, 
                varnam = "rp_diag", 
                breaks = c(seq(0, 500, by = 50), Inf), 
                latmin = -57, latmax = 80,
                spacing = "constant", 
                maxval = 6000, 
                combine = FALSE, 
                colorscale = viridis::magma, 
                legend_title = "",
                hillshade = FALSE,
                rivers = FALSE,
                ocean = TRUE,
                expand_size_y = 0.5,
                legend_direction = "horizontal"
                )
gg$ggmap <- gg$ggmap + labs(title = expression(italic(T)[EF]))
cowplot::plot_grid(gg$ggmap, gg$gglegend, ncol = 1, rel_heights = c(1, 0.3))
gg_fig4a <- gg$ggmap
gg_fig4_legend <- gg$gglegend
save(gg_fig4a, file = "data/gg_fig5a.Rdata")

ggsave("fig/map_rp_fet.pdf", width = 8, height = 5)
ggsave("fig/map_rp_fet.png", width = 8, height = 5)

load("data/df_rl_nSIF.RData")

# ## xxx debug
# load("data/df_rl/df_rl_fet_ichunk_TEST.RData")  # loads df
# df_rl_fet <- df

## function to determine the return period with the lowest bias (absolute)
find_rp_diag <- function(df){
  out <- df |> 
    group_by(return_period) |> 
    summarise(logbias = median(logbias, na.rm = TRUE)) |> 
    slice(which.min(abs(logbias))) |> 
    pull(return_period)
  out <- ifelse(length(out)==0, NA, out)
  return(out)
}

## bin into gridcells to be able to get a distribution (0.5 deg contains a maximum of 100 pixels)
dlon <- 1
dlat <- 1
lon_breaks <- seq(from = -180, to = 180, by = dlon)
lat_breaks <- seq(from = -90, to = 90, by = dlat)

## this takes about 1 min
df_rl_agg_nSIF <- df_rl_nSIF |>
  ungroup() |> 
  mutate(ilon = cut(lon, 
                    breaks = lon_breaks
                    # labels = as.character(seq(length(lon_breaks)-1))
                    ),
         ilat = cut(lat, 
                    breaks = lat_breaks
                    # labels = as.character(seq(length(lat_breaks)-1))
                    )
         ) |> 
  mutate(lon_lower = as.numeric( sub("\\((.+),.*", "\\1", ilon)),
         lon_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilon) ),
         lat_lower = as.numeric( sub("\\((.+),.*", "\\1", ilat) ),
         lat_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilat) )
         ) |> 
  mutate(lon_mid = (lon_lower + lon_upper)/2,
         lat_mid = (lat_lower + lat_upper)/2) |> 
  dplyr::select(-ilon, -ilat, -lon_lower, -lat_lower, -lat_upper, -lon_upper) |> 
  
  ## nest by gridcell and determine return period for which the absolute of the bias is lowest
  group_by(lon_mid, lat_mid) |> 
  nest() |> 
  ungroup() |> 
  mutate(rp_diag = purrr::map_dbl(data, ~find_rp_diag(.))) |> 
  dplyr::select(-data) |> 
  rename(lon = lon_mid, lat = lat_mid)

save(df_rl_agg_nSIF, file = "data/df_rl_agg_nSIF.RData")

## distribution of diagnosed return periods
df_rl_agg_nSIF |> 
  ggplot(aes(rp_diag, ..count..)) + 
  geom_histogram()

## global analysis of best return period
df_rl_nSIF |> 
  ggplot(aes(as.factor(return_period), logbias)) +
  # geom_violin(draw_quantiles = 0.5) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  ylim(-1.2, 1.2)

load("data/df_rl_agg_nSIF.RData")

# nc <- df_to_grid(
#   df_rl_agg, 
#   varnam = "rp_diag", 
#   lonnam = "lon", latnam = "lat"
#   )

df_rl_agg_nSIF <- df_rl_agg_nSIF |> 
  mutate(rp_diag = ifelse(lat > 75, NA, rp_diag))

gg <- plot_map4(df_rl_agg_nSIF, 
                varnam = "rp_diag", 
                breaks = c(seq(0, 500, by = 50), Inf), 
                latmin = -57, latmax = 80,
                spacing = "constant", 
                maxval = 6000, 
                combine = FALSE, 
                colorscale = viridis::magma, 
                legend_title = "(yr)",
                hillshade = FALSE,
                rivers = FALSE,
                ocean = TRUE,
                expand_size_y = 0.5)
gg$ggmap <- gg$ggmap + labs(title = expression(italic(T)[SIF]))

gg_fig4b <- gg$ggmap
save(gg_fig4b, file = "data/gg_fig5b.Rdata")

ggsave("fig/map_rp_nsif.pdf", width = 8, height = 5)
ggsave("fig/map_rp_nsif.pdf", width = 8, height = 5)

# ## missing lon-bands
# tmp <- df_rl_fet |>
#   ungroup() |> 
#   mutate(ilon = cut(lon, 
#                     breaks = lon_breaks
#                     # labels = as.character(seq(length(lon_breaks)-1))
#                     ),
#          ilat = cut(lat, 
#                     breaks = lat_breaks
#                     # labels = as.character(seq(length(lat_breaks)-1))
#                     )
#          ) |> 
#   mutate(lon_lower = as.numeric( sub("\\((.+),.*", "\\1", ilon)),
#          lon_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilon) ),
#          lat_lower = as.numeric( sub("\\((.+),.*", "\\1", ilat) ),
#          lat_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilat) )
#          ) |> 
#   mutate(lon_mid = (lon_lower + lon_upper)/2,
#          lat_mid = (lat_lower + lat_upper)/2) |> 
#   dplyr::select(-ilon, -ilat, -lon_lower, -lat_lower, -lat_upper, -lon_upper) |> 
#   group_by(lon_mid, lat_mid) |> 
#   nest() |> 
#   mutate(count = map_int(data, ~nrow(.)))
# 
# tmp |> 
#   dplyr::filter(lon_mid > 120 & lon_mid < 121)
#   # dplyr::filter(lon_mid == 120.5)

df_rl_agg <- df_rl_agg_nSIF |> 
  mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) |> 
  rename(rp_diag_nsif = rp_diag) |> 
  left_join(df_rl_agg_fet |> 
              mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) |> 
              rename(rp_diag_fet = rp_diag),
            by = c("lon", "lat")) |> 
  mutate(rp_diag = ifelse(is.na(rp_diag_nsif), 
                          rp_diag_fet, 
                          ifelse(is.na(rp_diag_fet), 
                                 rp_diag_nsif,
                                 (rp_diag_fet + rp_diag_nsif) / 2)))

save(df_rl_agg, file = "data/df_rl_agg.RData")

plot_map4(df_rl_agg, varnam = "rp_diag")
ggsave("fig/map_rl.pdf", width = 12, height = 8)

df_rl_agg_test <- df_rl_agg |> 
  mutate(lon = round(lon, digits = 1), lat = round(lat, digits = 1))

tmp_gti <- nc_to_df("~/data/gti_marthews/ga2_1deg.nc", varnam = "gti") |> 
      mutate(lat = lat - 0.1) |> 
      mutate(lon = round(lon, digits = 1), lat = round(lat, digits = 1)) |>
      mutate(gti = ifelse(gti < 0, NA, gti))

tmp_wtd <- nc_to_df("~/data/watertable_fan13sci/Global_wtd_lowres_1.0deg.nc", varnam = "WTD") |> 
      mutate(lon = round(lon, digits = 1), lat = round(lat, digits = 1))

tmp_fcf <- nc_to_df("~/data/modis_forestcover/MODIS-TERRA_C6__MOD44B__ForestCoverFraction__LPDAAC__GLOBAL__1.0degree__UHAM-ICDC__20100306__fv0.02.nc", varnam = "forestcoverfraction") |> 
          mutate(lon = round(lon, digits = 1), lat = round(lat, digits = 1))  |> 
          mutate(forestcoverfraction = forestcoverfraction / 100)
  
tmp_pll <- nc_to_df("~/data/pelletier/average_soil_and_sedimentary-deposit_thickness_1x1deg.nc", varnam = "layer") |> 
          mutate(lon = round(lon, digits = 1), lat = round(lat, digits = 1))  |> 
          rename(ssdth = layer)


df_rl_agg_test <- tmp_gti |> 
  left_join(tmp_fcf, by = c("lon", "lat")) |> 
  left_join(df_rl_agg_test, by = c("lon", "lat")) |> 
  left_join(tmp_wtd, by = c("lon", "lat")) |> 
  left_join(tmp_pll, by = c("lon", "lat")) |> 
  mutate(gti_bin = cut(gti, breaks = c(1:4, 10)),
         fcf_bin = cut(forestcoverfraction, c(seq(0, 0.5, by = 0.1), 0.9)),
         wtd_bin = cut(WTD, c(0, 5, 10, 15, 200)),
         ssdth_bin = cut(ssdth, seq(0, 50, by = 5))) |> 
  mutate(forest = ifelse(forestcoverfraction > 0.4, TRUE, FALSE))

save(df_rl_agg_test, file = "data/df_rl_agg_test.Rdata")

load("data/df_rl_agg_test.Rdata")

n_fun <- function(x){
  return(data.frame(y = 520,
                    label = length(x)))
}

gg_fig5c <- df_rl_agg_test |>
  drop_na(rp_diag) |> 
  drop_na(gti_bin, rp_diag) |> 
  ggplot(aes(x = gti_bin, y = rp_diag)) +
  geom_boxplot(fill = "grey70") +
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5, size = 3) +
  theme_classic() +
  ylim(0, 520) +
  labs(x = "CTI", y = "Return period (yr)")

gg_fig5d <- df_rl_agg_test |> 
  drop_na(fcf_bin, rp_diag) |> 
  ggplot(aes(x = fcf_bin, y = rp_diag)) +
  geom_boxplot(fill = "grey70") +
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5, size = 3) +
  theme_classic() +
  ylim(0, 520) +
  labs(x = "Forest cover fraction", y = "Return period (yr)")

gg_fig5e <- df_rl_agg_test |> 
  drop_na(ssdth_bin, rp_diag) |> 
  ggplot(aes(x = ssdth_bin, y = rp_diag)) +
  geom_boxplot(fill = "grey70") +
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5) +
  theme_classic() +
  labs(x = "Soil and sedimentary deposit thickness", y = "Return period (yr)")

gg_fig5c
gg_fig5d
gg_fig5e

plot_map3(df_rl_agg_test, varnam = "rp_diag")
plot_map3(df_rl_agg_test, varnam = "gti", breaks = c(1, seq(from = 2, to = 7, by = 0.5), 10))
plot_map3(df_rl_agg_test, varnam = "forestcoverfraction")
plot_map3(df_rl_agg_test, varnam = "test")
plot_map3(df_rl_agg_test, varnam = "WTD", breaks = c(0, 5, 10, 15, 20, 100, 200))

print("-----with WTD-------")
linmod <- lm(rp_diag ~ gti + forestcoverfraction + WTD, data = df_rl_agg_test)
summary(linmod)

print("-----without WTD-------")
linmod <- lm(rp_diag ~ gti + forestcoverfraction, data = df_rl_agg_test)
summary(linmod)

print("-----with soil and sedimentary deposit thickness-------")
linmod <- lm(rp_diag ~ gti + forestcoverfraction + WTD + ssdth, data = df_rl_agg_test)
summary(linmod)

library(ranger)
mod_rp_diag <- ranger(
  rp_diag ~ gti + forestcoverfraction, 
  data = df_rl_agg_test |> 
    ungroup() |> 
    dplyr::select(rp_diag, gti, forestcoverfraction) |> 
    drop_na(rp_diag, gti, forestcoverfraction),
  mtry = 1,
  respect.unordered.factors = "order",
  seed = 123
)

