#!/usr/bin/env Rscript

# Calculate global and site rooting depth
# Extracted from vignettes/archive/workflow_legacy.Rmd.
source("analysis/_common.R")

## WHC, prepared by analysis/05_soil/01_calculate_soil_parameters.R,
## analysis/05_soil/03_combine_soil_parameters.R, and code above writing "data/df_hwsd_hires.RData"
# load("data/df_whc_hires.RData")  # loads 'df_whc' - this has oddly missing values in south america
load("data/df_whc_hires_lasthope.RData")  # loads 'df_whc' - this one is complete

df_whc <- df_whc |> 
  dplyr::select(out) |> 
  unnest(out)
save(df_whc, file = "data/df_whc.RData")

## add biome info
df_biome <- nc_to_df("~/data/biomes/wwf_ecoregions/official/wwf_terr_ecos_raster.nc", varnam = "biome") |> 
  tidyr::drop_na(biome)

load("data/df_cwdx_10_20_80.RData") # loads 'df', created by analysis/04_cwd_extremes/04_collect_return_levels.R

## merge
df <- df |> 
  mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) |> 
  left_join(df_biome |> 
              mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)), 
            by = c("lon", "lat")) |> 
  left_join(df_whc |> 
              mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)), 
            by = c("lon", "lat"))

save(df, file = "data/df_mct_merged.RData")

gg <- df_whc |> 
  mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) |> 
  plot_map3(varnam = "whc_top", lonmin = -80, lonmax = -60, latmin = -60, latmax = -35, 
            breaks = c(seq(0, 0.4, by = 0.05), 0.8, Inf), 
            spacing = "constant",
            combine = FALSE, 
            colorscale = viridis::magma
            )
gg$ggmap <- gg$ggmap + labs(title = "WHC", subtitle = "Topsoil, fraction")
cowplot::plot_grid(gg$ggmap, gg$gglegend, ncol = 2, rel_widths = c(1, 0.2))

## WHC
# nc <- df_to_grid(df, varnam = "whc_top", lonnam = "lon", latnam = "lat")
# write_nc2(nc, varnams = "whc_top", lon = df$lon |> unique() |> sort(), lat = df$lat |> unique() |> sort(), path = "data/whc_top.nc", make_zdim = FALSE)
# system("cdo remapbil,data-raw/grid/gridfile_quartdeg.txt data/whc_top.nc data/whc_top_quartdeg.nc")

nc <- df_to_grid(df, varnam = "whc_sub", lonnam = "lon", latnam = "lat")
write_nc2(nc, varnams = "whc_sub", lon = df$lon |> unique() |> sort(), lat = df$lat |> unique() |> sort(), path = "data/whc_sub.nc", make_zdim = FALSE)
system("cdo remapbil,data-raw/grid/gridfile_quartdeg.txt data/whc_sub.nc data/whc_sub_quartdeg.nc")

## FC
# nc <- df_to_grid(df, varnam = "fc_top", lonnam = "lon", latnam = "lat")
# write_nc2(nc, varnams = "fc_top", lon = df$lon |> unique() |> sort(), lat = df$lat |> unique() |> sort(), path = "data/fc_top.nc", make_zdim = FALSE)
# system("cdo remapbil,data-raw/grid/gridfile_quartdeg.txt data/fc_top.nc data/fc_top_quartdeg.nc")

nc <- df_to_grid(df, varnam = "fc_sub", lonnam = "lon", latnam = "lat")
write_nc2(nc, varnams = "fc_sub", lon = df$lon |> unique() |> sort(), lat = df$lat |> unique() |> sort(), path = "data/fc_sub.nc", make_zdim = FALSE)
system("cdo remapbil,data-raw/grid/gridfile_quartdeg.txt data/fc_sub.nc data/fc_sub_quartdeg.nc")

## PWP
# nc <- df_to_grid(df, varnam = "pwp_top", lonnam = "lon", latnam = "lat")
# write_nc2(nc, varnams = "pwp_top", lon = df$lon |> unique() |> sort(), lat = df$lat |> unique() |> sort(), path = "data/pwp_top.nc", make_zdim = FALSE)
# system("cdo remapbil,data-raw/grid/gridfile_quartdeg.txt data/pwp_top.nc data/pwp_top_quartdeg.nc")

nc <- df_to_grid(df, varnam = "pwp_sub", lonnam = "lon", latnam = "lat")
write_nc2(nc, varnams = "pwp_sub", lon = df$lon |> unique() |> sort(), lat = df$lat |> unique() |> sort(), path = "data/pwp_sub.nc", make_zdim = FALSE)
system("cdo remapbil,data-raw/grid/gridfile_quartdeg.txt data/pwp_sub.nc data/pwp_sub_quartdeg.nc")

nc_quartdeg <- read_nc_onefile("data/whc_top_quartdeg.nc", varnam = "whc_top")
gg <- plot_map3(
  nc_quartdeg, 
  varnam = "whc_top", 
  breaks = c(seq(0, 0.4, by = 0.05), 0.8, Inf), 
  latmin = -60, latmax = 75,
  spacing = "constant",
  colorscale = viridis::magma,
  combine = FALSE
  )
gg$ggmap <- gg$ggmap + labs(title = "WHC", subtitle = "Topsoil, fraction")
cowplot::plot_grid(gg$ggmap, gg$gglegend, ncol = 2, rel_widths = c(1, 0.2))
ggsave("fig/map_whc_top.pdf", width = 10, height = 6)
ggsave("fig/map_whc_top.png", width = 10, height = 6)

source("R/calc_zroot.R")
load("data/df_mct_merged.RData")

df <- df |> 
  rowwise() |> 
  dplyr::mutate(#zroot_cwd10 = calc_zroot(cwdx10, whc_top, whc_sub),
                #zroot_cwd20 = calc_zroot(cwdx20, whc_top, whc_sub),
                #zroot_cwd40 = calc_zroot(cwdx40, whc_top, whc_sub),
                zroot_cwd80 = calc_zroot(cwdx80, whc_top, whc_sub),
                #zroot_cwd100 = calc_zroot(cwdx100, whc_top, whc_sub),
                #zroot_cwd200 = calc_zroot(cwdx200, whc_top, whc_sub),
                ) |> 
 ungroup() |> 
 dplyr::select(lon, lat, biome, starts_with("zroot_"))

save(df, file = "data/df_zroot80.RData")

## save this as a mask
df_mask <- df |> 
  mutate(mask = ifelse(!is.na(zroot_cwd80), 1, NA)) |> 
  dplyr::select(lon, lat, mask)
save(df_mask, file = "data/df_mask.RData")

load("data/df_zroot80.RData")

df |> 
  ggplot(aes(zroot_cwd80/1000, ..density..)) + 
  geom_density() +
  xlim(0, 30)

load("data/df_zroot80.RData")

nc <- df_to_grid(df, varnam = "zroot_cwd80", lonnam = "lon", latnam = "lat")
write_nc2(nc, varnams = "zroot_cwd80", lon = df$lon |> unique() |> sort(), lat = df$lat |> unique() |> sort(), path = "data/zroot_cwd80.nc", make_zdim = FALSE)

system("cdo remapbil,data-raw/grid/gridfile_quartdeg.txt data/zroot_cwd80.nc data/zroot_cwd80_quartdeg.nc")
system("cdo remapbil,data-raw/grid/gridfile_tenthdeg.txt data/zroot_cwd80.nc data/zroot_cwd80_tenthdeg.nc")

load("data/df_vegmask_tenthdeg.RData")  # loads df_vegmask_tenthdeg

nc_tenthdeg <- nc_to_df("data/zroot_cwd80_tenthdeg.nc", varnam = "zroot_cwd80") |> 
  mutate(lon = round(lon, digits = 2), lat = round(lat, digits = 2)) |>
  left_join(df_vegmask_tenthdeg |>
              mutate(lon = round(lon, digits = 2), lat = round(lat, digits = 2)),
            by = c("lon", "lat")) |>
  mutate(zroot_cwd80 = ifelse(nonveg > 99, NA, zroot_cwd80)) |>
  mutate(zroot_cwd80 = ifelse(lat > 74, NA, zroot_cwd80)) |>
  mutate(zroot_cwd80 = zroot_cwd80 / 1000)

gg <- plot_map4(nc_tenthdeg, 
                varnam = "zroot_cwd80", 
                breaks = c(seq(0, 1, by = 0.2), 1.5, 2, 3, 5, 7, 10, 15, 20, 25, Inf), 
                latmin = -60, latmax = 80,
                spacing = "constant", 
                combine = FALSE, 
                colorscale = "lapaz", 
                legend_title = "(m)",
                hillshade = FALSE,
                rivers = FALSE,
                ocean = TRUE,
                expand_size_y = 0.5,
                legend_direction = "vertical")
gg$ggmap <- gg$ggmap + 
  labs(title = expression(paste(italic("z")[CWDX80])))

# cowplot::plot_grid(gg$ggmap, gg$gglegend, ncol = 2, rel_widths = c(1, 0.12))
# ggsave("fig/map_zroot_cwd80.png", width = 10, height = 5)
# ggsave("fig/map_zroot_cwd80.pdf", width = 10, height = 5)

gg_fig3b <- gg$ggmap
gg_fig3b_legend <- gg$gglegend

load("data/df_zroot80.RData") # loads df

gg_hist_zr <- df |> 
  # filter(zroot_cwd80 < 25000) |>
  ggplot(aes(x = zroot_cwd80/1000)) +
  # geom_density()
  geom_histogram(binwidth = 0.5) +
  xlim(0, 30) + ylim(0, 1e6) +
  theme_classic() +
  labs(x = expression(italic(z)[CWDX80] ~ "(m)"), y = "Count")
gg_hist_zr

filn <- "data/df_zroot_sj02.RData"

if (!file.exists(filn)){

  ## use CWDXX for zroot, no root obstacles accounted for now
  df_zroot <- df_cwd |>
    ungroup() |> 
    dplyr::select(sitename = idx, out_mct) |> 
    left_join(
      df_whc |> 
        unnest(data_soiltext_sub) |>
        dplyr::select(sitename, whc_s = whc),
      by = "sitename"
    ) |> 
    left_join(
      df_whc |> 
        unnest(data_soiltext_top) |>
        dplyr::select(sitename, whc_t = whc, roots, imperm),
      by = "sitename"
    ) |> 
    # dplyr::select(sitename) |> 
    
    ## rooting depth from 20-year extreme
    dplyr::mutate(cwdx10 = purrr::map_dbl(out_mct, ~extract_return_level(., 10)),
                  cwdx20 = purrr::map_dbl(out_mct, ~extract_return_level(., 20)),
                  cwdx30 = purrr::map_dbl(out_mct, ~extract_return_level(., 30)),
                  cwdx40 = purrr::map_dbl(out_mct, ~extract_return_level(., 40))
                  ) |> 

    ## rooting depth from 10-year extreme
    rowwise() |> 
    dplyr::mutate(zroot_cwd10 = calc_zroot(cwdx10, whc_t, whc_s, roots, imperm),
                  zroot_cwd20 = calc_zroot(cwdx20, whc_t, whc_s, roots, imperm),
                  zroot_cwd30 = calc_zroot(cwdx30, whc_t, whc_s, roots, imperm),
                  zroot_cwd40 = calc_zroot(cwdx40, whc_t, whc_s, roots, imperm)
                  )

  save(df_zroot, file = filn)
  
} else {
  
  load(filn)
  
}

load("data/siteinfo_sj02.RData")

df_sj02 <- read_csv("~/data/rootingdepth/root_profiles_schenkjackson02/data/root_profiles_D50D95.csv") |> 
  dplyr::rename(lon = Longitude, lat = Latitude) |> 
  dplyr::filter(lon > -180 & lat > -90) |> 
  dplyr::filter(Wetland == "N" & Anthropogenic == "N" & Schenk_Jackson_2002 == "YES") |> 
  dplyr::rename(sitename = ID) |> 
  dplyr::mutate(D50 = 1000 * D50, D95 = 1000 * D95, D50_extrapolated = 1000 * D50_extrapolated, D95_extrapolated = 1000 * D95_extrapolated)

## extract values from global simulation
df_sj02 <- df_sj02 |>   
  left_join(
    extract_pointdata_allsites("data/zroot_cwd40.nc", df_sj02 |> dplyr::select(lon, lat), time = FALSE) |> 
      rename(zroot_cwdx40 = NA.),
    by = c("lon", "lat")
  ) |> 

  ## add biome info
  left_join(siteinfo |> 
              dplyr::select(sitename, biome_name = biome_name),
            by = "sitename")

save(df_sj02, file = paste0("data/df_sj02_biome_wwf_NEW.Rdata"))

library(ggridges)
#load("data/df_sj02_biome_wwf.Rdata")

df_sj02 |> 
  dplyr::select(sitename, biome_name, obs = D95_extrapolated, mod = zroot_cwdx40, lon, lat) |>  # change to mod = zroot_cwdx20_global
  tidyr::pivot_longer(cols = c(mod, obs), names_to = "source", values_to = "zroot") |> 
  dplyr::filter(!is.na(biome_name)) |> 
  dplyr::filter(biome_name!="Mangroves") |>   
  ggplot(aes(x = zroot / 1000, y = biome_name, color = source, point_color = source, fill = source)) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = "|", point_size = 1.5, size = 0.25,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0), name = "") +
  scale_x_continuous(expand = c(0, 0), name = "Rooting depth (m)") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c(expression(italic(z)[CWDX40]), "Observed"), name = "") +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA)
    )
  ) +
  labs(title = expression("Rooting depth, SJ02 data")) +
  theme_ridges(center = TRUE)

ggsave("fig/modobs_ridges_zroot_biome_wwf_2022.pdf", width = 15, height = 10)
ggsave("fig/modobs_ridges_zroot_biome_wwf_2022.png", width = 15, height = 10)

#load("data/df_sj02_biome_wwf_NEW.Rdata")

df_corr_biome <- df_sj02 |> 
  dplyr::filter(biome_name != "Mangroves") |> 
  group_by(biome_name) |> 
  summarise(q10_zroot_cwdx40 = quantile(zroot_cwdx40, probs = 0.10, na.rm = TRUE)/1000,
            q25_zroot_cwdx40 = quantile(zroot_cwdx40, probs = 0.25, na.rm = TRUE)/1000,
            q50_zroot_cwdx40 = quantile(zroot_cwdx40, probs = 0.50, na.rm = TRUE)/1000,
            q75_zroot_cwdx40 = quantile(zroot_cwdx40, probs = 0.75, na.rm = TRUE)/1000,
            q90_zroot_cwdx40 = quantile(zroot_cwdx40, probs = 0.90, na.rm = TRUE)/1000,
            
            q10_zroot_obs = quantile(D95_extrapolated, probs = 0.10, na.rm = TRUE)/1000,
            q25_zroot_obs = quantile(D95_extrapolated, probs = 0.25, na.rm = TRUE)/1000,
            q50_zroot_obs = quantile(D95_extrapolated, probs = 0.50, na.rm = TRUE)/1000,
            q75_zroot_obs = quantile(D95_extrapolated, probs = 0.75, na.rm = TRUE)/1000,
            q90_zroot_obs = quantile(D95_extrapolated, probs = 0.90, na.rm = TRUE)/1000
            )

out <- df_corr_biome |> 
  analyse_modobs2("q10_zroot_cwdx40", "q10_zroot_obs", label = FALSE, id = "biome_name", nlabels = 3)
gg_sj02_10 <- out$gg + labs(title = "10% quantile by biome, SJ02 data",
              x = expression(italic(z)[CWDX40] ~ " (m)"),
              y = expression("Observed" ~ italic(z)[root] ~ " (m)"))
gg_sj02_10
save(gg_sj02_10, file = "data/gg_sj02_10.RData")

out <- df_corr_biome |> 
  analyse_modobs2("q25_zroot_cwdx40", "q25_zroot_obs", label = FALSE, id = "biome_name", nlabels = 3)
gg_sj02_25 <- out$gg + labs(title = "25% quantile by biome, SJ02 data",
              x = expression(italic(z)[CWDX40] ~ " (m)"),
              y = expression("Observed" ~ italic(z)[root] ~ " (m)"))
gg_sj02_25
save(gg_sj02_25, file = "data/gg_sj02_25.RData")

out <- df_corr_biome |> 
  analyse_modobs2("q50_zroot_cwdx40", "q50_zroot_obs", label = FALSE, id = "biome_name", nlabels = 3)
gg_sj02_50 <- out$gg + labs(title = "50% quantile by biome, SJ02 data",
              x = expression(italic(z)[CWDX40] ~ " (m)"),
              y = expression("Observed" ~ italic(z)[root] ~ " (m)"))
gg_sj02_50
save(gg_sj02_50, file = "data/gg_sj02_50.RData")

out <- df_corr_biome |> 
  analyse_modobs2("q75_zroot_cwdx40", "q75_zroot_obs", label = FALSE, id = "biome_name", nlabels = 3)
gg_sj02_75 <- out$gg + labs(title = "75% quantile by biome, SJ02 data",
              x = expression(italic(z)[CWDX40] ~ " (m)"),
              y = expression("Observed" ~ italic(z)[root] ~ " (m)"))
gg_sj02_75
save(gg_sj02_75, file = "data/gg_sj02_75.RData")

out <- df_corr_biome |> 
  analyse_modobs2("q90_zroot_cwdx40", "q90_zroot_obs", label = FALSE, id = "biome_name", nlabels = 3)
gg_sj02_90 <- out$gg + labs(title = "90% quantile by biome, SJ02 data",
              x = expression(italic(z)[CWDX40] ~ " (m)"),
              y = expression("Observed" ~ italic(z)[root] ~ " (m)"))
gg_sj02_90
save(gg_sj02_90, file = "data/gg_sj02_90.RData")

df_tmp <- df_modobs |> 
  mutate(error = zroot - D95_extrapolated) |> 
  arrange(desc(error)) |> 
  dplyr::select(sitename, zroot, D95_extrapolated, error, biome_wwf, lon, lat)

plot_map_simpl() +
  geom_point(data = df_tmp, 
             aes(lon, lat, color = error)) +
  scale_colour_gradient(low = "grey30", high = "red")

df_modobs |> 
  left_join(siteinfo |> 
              dplyr::select(sitename, biome_wwf = biome_name),
            by = "sitename") |> 
  mutate(error = zroot - D95_extrapolated) |> 
  arrange(desc(error)) |> 
  dplyr::select(sitename, zroot, D95_extrapolated, error, biome_wwf) |> 
  left_join(df_sif |> 
              dplyr::select(sitename = idx, flue, cwdmax),
            by = "sitename") |> 
  ggplot(aes(x = flue, y = error)) +
  geom_point()

df_sif |> 
  rename(sitename = idx) |> 
  left_join(siteinfo |> 
              dplyr::select(sitename, biome_wwf = biome_name),
            by = "sitename") |> 
  tidyr::pivot_longer(cols = c(cwdx20, cwd_lue0), names_to = "source", values_to = "cwd") |> 
  dplyr::filter(!is.na(biome_wwf)) |> 
  dplyr::filter(biome_wwf!="Mangroves") |> 

  ggplot(aes(x = cwd, y = biome_wwf, color = source, point_color = source, fill = source)) +
  geom_density_ridges(
    jittered_points = FALSE, scale = .95, rel_min_height = .01,
    point_shape = "|", point_size = 1.5, size = 0.25,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), name = "rooting depth (mm)") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("CWD(LUE=0)", "CWDX")) +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA)
    )
  ) +
  ggtitle("Rooting depth by WWF biomes, SJ02 sites") +
  theme_ridges(center = TRUE)

ggsave(paste0("fig/modobs_ridges_cwd_biome_wwf_NEW", siteset, ".pdf"), width = 15, height = 10)

load("data/df_modobs_reOLD.Rdata")
df_modobs_new <- dplyr::select(df_modobs, sitename, cwdx20, zroot)

df_test <- df_modobs_reOLD |>
  dplyr::select(zroot_old = zroot, cwdx20_old = cwd20) |>
  left_join(df_modobs_new, by = "sitename")

out <- df_test |>
  analyse_modobs2("zroot_old", "zroot")
out$gg

out <- df_test |>
  analyse_modobs2("cwdx20_old", "cwdx20")
out$gg
