#!/usr/bin/env Rscript

# Analyse bias and create publication figures
# Extracted from vignettes/archive/workflow_legacy.Rmd.
source("analysis/_common.R")

## read groundwater table maps per continent
## africa
df_wtd <- nc_to_df("~/data/watertable_fan13sci/Africa_model_wtd_v2_0.05deg_CLEAN.nc", varnam = "WTD") |> 
  rename(wtd = myvar) |> 
  mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) |>
  drop_na() |> 

  ## australia
  bind_rows(
    nc_to_df("~/data/watertable_fan13sci/Australia_model_wtd_v2_0.05deg_CLEAN.nc", varnam = "WTD") |> 
      rename(wtd = myvar) |> 
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) |>
      drop_na()
    ) |> 

  ## eurasia
  bind_rows(
    nc_to_df("~/data/watertable_fan13sci/Eurasia_model_wtd_v2_0.05deg_CLEAN.nc", varnam = "WTD") |> 
      rename(wtd = myvar) |> 
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) |>
      drop_na()
    ) |> 

  ## n america
  bind_rows(
    nc_to_df("~/data/watertable_fan13sci/N_America_model_wtd_v2_0.05deg_CLEAN.nc", varnam = "WTD") |> 
      rename(wtd = myvar) |> 
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) |>
      drop_na()
    ) |> 

  ## s america
  bind_rows(
    nc_to_df("~/data/watertable_fan13sci/S_America_model_wtd_v2_0.05deg_CLEAN.nc", varnam = "WTD") |> 
      rename(wtd = myvar) |> 
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) |>
      drop_na()
    )

save(df_wtd, file = "data/df_wtd.RData")

load("data/df_corr.RData")
load("data/df_wtd.RData")

# nc_vegtype <- read_nc_onefile("~/data/landcover/modis_landcover__LPDAAC__v5.1__0.05deg__2010.nc", varnam = "landcover")

df_pred <- df_corr |> 
  mutate(bias_40 = -log(cwdx40 / cwd_lue0_SIF)) |> 
  dplyr::select(lon, lat, cwdx40, cwd_lue0_SIF, cwd_lue0_fet, bias_40) |> 
  drop_na() |> 

  ## read GTI
  left_join(
    nc_to_df("~/data/gti_marthews/ga2_0_05deg.nc", varnam = "gti") |> 
      drop_na() |> 
      rename(gti = myvar) |> 
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)),
    by = c("lon", "lat")
  ) |> 
  
  # ## read vegetation map
  # left_join(
  #   nc_to_df(nc_vegtype, varnam = "landcover") |> 
  #     rename(vegtype = myvar) |> 
  #     mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)) |> 
  #     dplyr::filter(vegtype > 0) |> 
  #     drop_na(),
  #   by = c("lon", "lat")
  # ) |> 

  ## add wtd map
  left_join(
    df_wtd |> 
      drop_na(),
    by = c("lon", "lat")
  ) |> 

  ## read irrigation map
  left_join(
    df_irr <- nc_to_df("~/data/irrigation/gmia_v5_aai_pct_0_05deg.nc", varnam = "aai") |> 
      drop_na() |> 
      rename(aai = myvar) |> 
      mutate(lon = round(lon, digits = 3), lat = round(lat, digits = 3)),
    by = c("lon", "lat")
  )

save(df_pred, file = "data/df_pred.RData")  

df_pred |> 
  mutate(cwd_lue0_SIF = remove_outliers(cwd_lue0_SIF, coef = 10)) |> 
  ggplot(aes(x = cwd_lue0_SIF, y = ..count..)) +
  geom_histogram()

load("data/df_pred.RData")

df_pred <- df_pred |> 
  mutate(cwd_lue0_SIF = remove_outliers(cwd_lue0_SIF, coef = 10)) |> 
  left_join(df_corr |> dplyr::select(lon, lat, forest), by = c("lon", "lat")) |> 
  drop_na()

df_pred |> 
  ggplot(aes(aai, bias_40)) +
  geom_hex(bins = 100) +
  scale_fill_gradientn(
    colours = colorRampPalette( c("gray65", "navy", "red", "yellow"))(5)) +
  geom_hline(yintercept = 0, linetype = "dotted")

df_pred |> 
  ggplot(aes(gti, bias_40)) +
  geom_hex(bins = 100) +
  scale_fill_gradientn(
    colours = colorRampPalette( c("gray65", "navy", "red", "yellow"))(5)) +
  geom_hline(yintercept = 0, linetype = "dotted")

df_pred |> 
  ggplot(aes(wtd, bias_40)) +
  geom_hex(bins = 100) +
  scale_fill_gradientn(
    colours = colorRampPalette( c("gray65", "navy", "red", "yellow"))(5)) +
  geom_hline(yintercept = 0, linetype = "dotted")

df_pred |> 
  ggplot(aes(forest, bias_40)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dotted")

library(recipes)
library(caret)

load("data/df_pred.RData")

df_sub <- df_pred |> 
  
  ## remove pixels with a lot of irrigation areas
  mutate(aai = ifelse(is.na(aai), 0, aai)) |> 
  dplyr::filter(aai < 0.5) |> 
  
  # ## central asia only
  # dplyr::filter(lon > 45 & lon < 95 & lat > 25 & lat < 47.5) |> 

  # mutate(cwd_lue0_SIF = remove_outliers(cwd_lue0_SIF, coef = 10)) |> 
  left_join(df_corr |> dplyr::select(lon, lat, forest), by = c("lon", "lat")) |> 
  mutate(forest = as.factor(forest)) |> 
  mutate(cwd_lue0_SIF_log = log(cwd_lue0_SIF), cwdx40_log = log(cwdx40))

linmod1 <- lm(cwd_lue0_SIF ~ cwdx40 + gti + wtd + factor(forest), data = df_sub)
summary(linmod1)

linmod2 <- lm(cwd_lue0_SIF_log ~ cwdx40_log + gti + wtd + factor(forest), data = df_sub)
summary(linmod2)

linmod3 <- lm(log(cwd_lue0_SIF) ~ log(cwdx40), data = df_sub)
summary(linmod3)

linmod4 <- lm(cwd_lue0_SIF_log ~ cwdx40_log, data = df_sub)
summary(linmod4)

linmod2 <- lm(cwd_lue0_SIF_log ~ cwdx40, data = df_sub)
summary(linmod2)

## with caret
myrecipe <- recipe(cwd_lue0_SIF ~ cwdx40 + gti + wtd  + forest, data = df_sub) |> 
  step_dummy(forest) |> 
  step_BoxCox(cwd_lue0_SIF, cwdx40)

traincotrlParams <- caret::trainControl( method = "none",
                                         savePredictions = "final"   # predictions on each validation resample are then available as modl$pred$Resample
                                         )
modl <- train(myrecipe,
              data = df_sub,
              method = "lm",
              trControl = traincotrlParams
              )

save(modl, file = "data/modl_lm_allvars.RData")

df_sub$.pred1 <- predict(linmod1, newdata = df_sub)
df_sub$.pred2 <- predict(linmod2, newdata = df_sub)

out <- df_sub |> 
  # dplyr::filter(forest) |> 
  # mutate(cwd_lue0_SIF = remove_outliers(cwd_lue0_SIF, coef = 10)) |> 
  rbeni::analyse_modobs2(".pred2", "cwd_lue0_SIF_log", type = "hex", plot_linmod = FALSE)
gg40 <- out$gg +
  scale_x_log10() + 
  scale_y_log10() +
  labs(title = "Full model", 
       x = expression(italic(S)[FULL] ~ " (mm)"), 
       y = expression(italic(S)[dSIF] ~ " (mm)")) +
  ylim(0.1, 12) + xlim(0.1, 12)
gg40
ggsave("fig/corr_FULL.pdf", width = 6, height = 5)
ggsave("fig/corr_FULL.png", width = 6, height = 5)

library(caret)
library(recipes)

load("data/df_pred.RData")

df_sub <- df_pred |> 
  mutate(aai = ifelse(is.na(aai), 0, aai)) |> 
  dplyr::filter(aai < 0.5) |> 
  
  ## central asia only
  dplyr::filter(lon > 45 & lon < 95 & lat > 25 & lat < 47.5) |> 
  mutate(cwd_lue0_SIF = remove_outliers(cwd_lue0_SIF, coef = 10)) |> 
  left_join(df_corr |> dplyr::select(lon, lat, forest), by = c("lon", "lat")) |> 
  drop_na()

set.seed(123)  # for reproducibility
index <- createDataPartition(df_pred$cwdx40, p = 0.7, list = FALSE)
df_train <- df_sub |> 
  slice(index)
df_test <- df_sub |> 
  slice(-index)

traincotrlParams <- caret::trainControl( method = "cv",
                                         number = 5,
                                         savePredictions = "final"   # predictions on each validation resample are then available as modl$pred$Resample
                                         )

hyper_grid <- expand.grid(.mtry = c(2),
                          .splitrule = "variance",
                          .min.node.size = c(10)
                          )

# hyper_grid <- expand.grid( .decay = c(0.1), .size = 20 )

# myrecipe <- recipe(cwd_lue0_SIF ~ cwdx40 + gti + wtd + aai + forest, data = df_pred) |>
#       step_center(all_numeric(), -all_outcomes()) |>
#       step_scale(all_numeric(), -all_outcomes()) |>
#       step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE)

myrecipe <- recipe(cwd_lue0_SIF ~ cwdx40 + gti + wtd + aai + forest, data = df_sub)

modl <- train(myrecipe,
              data = df_train,
              method = "ranger", # "nnet", # 
              trControl = traincotrlParams,
              tuneGrid = hyper_grid,
              metric = "RMSE",
              importance = "permutation"  # ranger-specific, enables VIP
              )

modl

save(modl, file = "data/modl_rf_allvars_casia.RData")

df_test$.pred <- predict(modl, newdata = df_test)

df_test |> 
  analyse_modobs2(".pred", "cwd_lue0_SIF", type = "heat")

library(pdp)
df_pd_gti <- partial(modl, pred.var = "gti")
df_pd_aai <- partial(modl, pred.var = "aai")
df_pd_wtd <- partial(modl, pred.var = "wtd")
df_pd_forest <- partial(modl, pred.var = "forest")

save(df_pd_gti, file = "data/df_pd_gti.RData")
save(df_pd_aai, file = "data/df_pd_aai.RData")
save(df_pd_wtd, file = "data/df_pd_wtd.RData")
save(df_pd_forest, file = "data/df_pd_forest.RData")

df_pd_gti |> 
  ggplot(aes(gti, yhat)) +
  geom_line()

df_pd_aai |> 
  ggplot(aes(aai, yhat)) +
  geom_line()

df_pd_wtd |> 
  ggplot(aes(wtd, yhat)) +
  geom_line()

df_pd_forest |> 
  ggplot(aes(forest, yhat)) +
  geom_point()

library(vip)
vip(modl$finalModel)

nc_landcover <- read_nc_onefile("~/data/landcover/modis_landcover__LPDAAC__v5.1__0.05deg__2010.nc", varnam = "landcover")

igbp_cols <- function(x, direction = 1){

  library(plotKML)
  library(gplots)

  data(worldgrids_pal)

  worldgrids_pal$IGBP["Water "] <- col2hex("lightblue1")  # rgb(1,0,0)
  worldgrids_pal$IGBP["Unclassified "] <- col2hex("white")
  worldgrids_pal$IGBP["Fill Value "] <- col2hex("white")
  worldgrids_pal$IGBP["Evergreen Broadleaf forest "] <- col2hex("forestgreen")
  worldgrids_pal$IGBP["Evergreen Needleleaf forest "] <- col2hex("dodgerblue4")
  worldgrids_pal$IGBP["Deciduous Needleleaf forest "] <- col2hex("dodgerblue2")
  worldgrids_pal$IGBP["Deciduous Broadleaf forest "] <- col2hex("springgreen3")
  worldgrids_pal$IGBP["Woody savannas "] <- col2hex("darkorange")
  worldgrids_pal$IGBP["Grasslands "] <- col2hex("khaki3")
  worldgrids_pal$IGBP["Permanent wetlands "] <- col2hex("violetred")
  # worldgrids_pal$IGBP["Mixed forest "] <- col2hex("springgreen4")

  return(unname(worldgrids_pal$IGBP[1:17]))
}

gg <- nc_landcover |>
  nc_to_df(varnam = "landcover") |>
  plot_map3(varnam = "myvar", lonmin = 45, lonmax = 95, latmin = 25, latmax = 47.5,
            spacing = "constant",
            breaks = 0:17,
            combine = FALSE,
            colorscale = igbp_cols,
            legend_title = ""
            )


# filnam <- "~/data/MODIS_Land-Cover-Type-MCD12Q1/v051/500m_sinusoidal_1y/processed/regrid/modis.land-cover-type.2010.tif"
filnam <- "~/data/landcover/modis_landcover__LPDAAC__v5.1__0.05deg__2010.nc"
rasta <- raster( filnam )

## change spatial resolution of rasta object
mostfrequent <- function(vec, na.rm=TRUE) { 
  out <- as.numeric(names(which.max(table(vec))))
  return(out) 
}
rasta_agg <- aggregate( x = rasta, fact = 10, fun = mostfrequent  )

plot_map3(rasta_agg)


## write raster object to netcdf
writeRaster( mybrick, filename="landcover_modis_2010.nc", format="CDF" )

plot( rasta_agg, col=worldgrids_pal$IGBP, legend=FALSE, axes=FALSE )  # , xlim = c(-10, 15), ylim = c(39, 55)
plot( rasta_agg,
  legend.only   =TRUE,
  col           =worldgrids_pal$IGBP,
  legend.width  =1,
  legend.shrink =0.75,
  axis.args     =list(at=c(0:length(worldgrids_pal$IGBP)),labels=as.character(c(0:length(worldgrids_pal$IGBP))),cex.axis=0.6),
  legend.args   =list(text='IGBP land cover class',side=4,font=2,line=2.5,cex=1.0)
  )

## central asia cutout
df_gti <- nc_to_df("~/data/gti_marthews/ga2_casia_05deg.nc", varnam = "Band1") |> 
  rename(gti = Band1) |> 
  left_join(df_corr_bias, by = c("lon", "lat"))

df_gti |> 
  analyse_modobs2("gti", "bias_20", type = "hex")

save(df_gti, file = "data/df_gti.RData")

load("data/df_corr.RData")

## 80 year return period
out <- df_corr |> 
  filter(cwdx80 < 2500) |> 
  # mutate(cwd_lue0_nSIF = remove_outliers(cwd_lue0_nSIF, coef = 10)) |> 
  analyse_modobs2("cwdx80", "cwd_lue0_nSIF", type = "hex", plot_linmod = FALSE)

gg80_sif <- out$gg +
  scale_x_continuous(expand = c(0,0), limits = c(0,1200)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,1200)) +
  labs(x = expression(italic(S)[CWDX80] ~ " (mm)"), 
       y = expression(italic(S)[dSIF] ~ " (mm)")) +
  scale_fill_gradientn(
        colours = colorRampPalette( c("gray65", "navy", "red", "yellow"))(5), 
        trans = "log", breaks = c(1, 10, 100, 1000, 10000))
gg80_sif
ggsave("fig/corr_cwd_lue0_nSIF_cwdx80.pdf", width = 6, height = 5)
ggsave("fig/corr_cwd_lue0_nSIF_cwdx80.png", width = 6, height = 5)

## 80 year return period
out <- df_corr |> 
  dplyr::filter(forest) |>
  mutate(cwd_lue0_fet = remove_outliers(cwd_lue0_fet, coef = 10)) |> 
  analyse_modobs2("cwdx80", "cwd_lue0_fet", type = "hex", plot_linmod = FALSE)
gg80_ef <- out$gg +
  scale_x_continuous(expand = c(0,0), limits = c(0,1200)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,1200)) +
  labs(x = expression(italic(S)[CWDX80] ~ " (mm)"), 
       y = expression(italic(S)[dEF] ~ " (mm)")) +
  scale_fill_gradientn(
        colours = colorRampPalette( c("gray65", "navy", "red", "yellow"))(5), 
        trans = "log", breaks = c(1, 10, 100, 1000, 10000))
gg80_ef
ggsave("fig/corr_cwd_lue0_fet_cwdx80.pdf", width = 6, height = 5)
ggsave("fig/corr_cwd_lue0_fet_cwdx80.png", width = 6, height = 5)

## publication figure
plot_grid(gg80_sif, gg80_ef, ncol = 2, labels = c('a', 'b'))
ggsave("fig/corr_cwd_lue0_fet_sif_cwdx80.pdf", width = 12, height = 5)
ggsave("fig/corr_cwd_lue0_fet_sif_cwdx80.png", width = 12, height = 5)

load("data/df_corr.RData")

df_corr_biome <- df_corr |> 
  dplyr::filter(biome_name != "Mangroves") |> 
  group_by(biome_name) |> 
  summarise(q10_cwdx80 = quantile(cwdx80, probs = 0.10, na.rm = TRUE),
            q25_cwdx80 = quantile(cwdx80, probs = 0.25, na.rm = TRUE),
            q50_cwdx80 = quantile(cwdx80, probs = 0.50, na.rm = TRUE),
            q75_cwdx80 = quantile(cwdx80, probs = 0.75, na.rm = TRUE),
            q90_cwdx80 = quantile(cwdx80, probs = 0.90, na.rm = TRUE),
            
            q10_cwd_lue0_SIF = quantile(cwd_lue0_SIF, probs = 0.10, na.rm = TRUE),
            q25_cwd_lue0_SIF = quantile(cwd_lue0_SIF, probs = 0.25, na.rm = TRUE),
            q50_cwd_lue0_SIF = quantile(cwd_lue0_SIF, probs = 0.50, na.rm = TRUE),
            q75_cwd_lue0_SIF = quantile(cwd_lue0_SIF, probs = 0.75, na.rm = TRUE),
            q90_cwd_lue0_SIF = quantile(cwd_lue0_SIF, probs = 0.90, na.rm = TRUE),
            
            q10_cwd_lue0_fet = quantile(cwd_lue0_fet, probs = 0.10, na.rm = TRUE),
            q25_cwd_lue0_fet = quantile(cwd_lue0_fet, probs = 0.25, na.rm = TRUE),
            q50_cwd_lue0_fet = quantile(cwd_lue0_fet, probs = 0.50, na.rm = TRUE),
            q75_cwd_lue0_fet = quantile(cwd_lue0_fet, probs = 0.75, na.rm = TRUE),
            q90_cwd_lue0_fet = quantile(cwd_lue0_fet, probs = 0.90, na.rm = TRUE)
            )

## Median works particularly generally well!
## dSIF
out <- df_corr_biome |> 
  analyse_modobs2("q10_cwdx80", "q10_cwd_lue0_SIF", label = FALSE, id = "biome_name", nlabels = 3)
gg_sif_10 <- out$gg + labs(title = "10% quantile by biome",
              x = expression(italic(S)[CWDX80] ~ " (mm)"),
              y = expression(italic(S)[dSIF] ~ " (mm)"))

out <- df_corr_biome |> 
  analyse_modobs2("q25_cwdx80", "q25_cwd_lue0_SIF", label = FALSE, id = "biome_name", nlabels = 3)
gg_sif_25 <- out$gg + labs(title = "25% quantile by biome",
              x = expression(italic(S)[CWDX80] ~ " (mm)"),
              y = expression(italic(S)[dSIF] ~ " (mm)"))

out <- df_corr_biome |> 
  analyse_modobs2("q50_cwdx80", "q50_cwd_lue0_SIF", label = FALSE, id = "biome_name", nlabels = 3)
gg_sif_50 <- out$gg + labs(title = "50% quantile by biome",
              x = expression(italic(S)[CWDX80] ~ " (mm)"),
              y = expression(italic(S)[dSIF] ~ " (mm)"))

out <- df_corr_biome |> 
  analyse_modobs2("q75_cwdx80", "q75_cwd_lue0_SIF", label = FALSE, id = "biome_name", nlabels = 3)
gg_sif_75 <- out$gg + labs(title = "75% quantile by biome",
              x = expression(italic(S)[CWDX80] ~ " (mm)"),
              y = expression(italic(S)[dSIF] ~ " (mm)"))

out <- df_corr_biome |> 
  analyse_modobs2("q90_cwdx80", "q90_cwd_lue0_SIF", label = FALSE, id = "biome_name", nlabels = 3)
gg_sif_90 <- out$gg + labs(title = "90% quantile by biome",
              x = expression(italic(S)[CWDX80] ~ " (mm)"),
              y = expression(italic(S)[dSIF] ~ " (mm)"))

## dEF
out <- df_corr_biome |> 
  analyse_modobs2("q10_cwdx80", "q10_cwd_lue0_fet", label = FALSE, id = "biome_name", nlabels = 3)
gg_ef_10 <- out$gg + labs(title = "10% quantile by biome",
              x = expression(italic(S)[CWDX80] ~ " (mm)"),
              y = expression(italic(S)[dEF] ~ " (mm)"))

out <- df_corr_biome |> 
  analyse_modobs2("q25_cwdx80", "q25_cwd_lue0_fet", label = FALSE, id = "biome_name", nlabels = 3)
gg_ef_25 <- out$gg + labs(title = "25% quantile by biome",
              x = expression(italic(S)[CWDX80] ~ " (mm)"),
              y = expression(italic(S)[dEF] ~ " (mm)"))

out <- df_corr_biome |> 
  analyse_modobs2("q50_cwdx80", "q50_cwd_lue0_fet", label = FALSE, id = "biome_name", nlabels = 3)
gg_ef_50 <- out$gg + labs(title = "50% quantile by biome",
              x = expression(italic(S)[CWDX80] ~ " (mm)"),
              y = expression(italic(S)[dEF] ~ " (mm)"))

out <- df_corr_biome |> 
  analyse_modobs2("q75_cwdx80", "q75_cwd_lue0_fet", label = FALSE, id = "biome_name", nlabels = 3)
gg_ef_75 <- out$gg + labs(title = "75% quantile by biome",
              x = expression(italic(S)[CWDX80] ~ " (mm)"),
              y = expression(italic(S)[dEF] ~ " (mm)"))

out <- df_corr_biome |> 
  analyse_modobs2("q90_cwdx80", "q90_cwd_lue0_fet", label = FALSE, id = "biome_name", nlabels = 3)
gg_ef_90 <- out$gg + labs(title = "90% quantile by biome",
              x = expression(italic(S)[CWDX80] ~ " (mm)"),
              y = expression(italic(S)[dEF] ~ " (mm)"))

save(gg_sif_50, file = "data/gg_sif_50.RData")
save(gg_ef_50, file = "data/gg_ef_50.RData")

library(patchwork)
load("data/80.RData")
load("data/gg_rsip_25.RData")
load("data/gg_rsip_50.RData")
load("data/gg_rsip_75.RData")
load("data/gg_rsip_90.RData")

load("data/gg_sj02_10.RData")
load("data/gg_sj02_25.RData")
load("data/gg_sj02_50.RData")
load("data/gg_sj02_75.RData")
load("data/gg_sj02_90.RData")

(gg_sj02_10 + gg_sj02_50 + gg_sj02_90) /
  (gg_rsip_10 + gg_rsip_50 + gg_rsip_90) /
  (gg_sif_10 + gg_sif_50 + gg_sif_90) /
  (gg_ef_10 + gg_ef_50 + gg_ef_90) + 
  plot_annotation(tag_levels = 'a') &  # , tag_prefix = '(', tag_suffix = ')'
  theme(plot.tag = element_text(face = "bold"))

ggsave("fig/modobs_quantiles_biome.pdf", width = 15, height = 15)
ggsave("fig/modobs_quantiles_biome.png", width = 15, height = 15)

load("data/df_corr.RData")

df_corr |> 
  dplyr::select(biome_name, obs = cwd_lue0_SIF, mod = cwdx40) |>
  drop_na() |> 
  tidyr::pivot_longer(cols = c(mod, obs), names_to = "source", values_to = "cwdx") |> 
  dplyr::filter(biome_name!="Mangroves") |>   
  ggplot(
    aes(x = cwdx, y = biome_name, color = source, point_color = source, fill = source)
    ) +
  geom_density_ridges(jittered_points = FALSE) +
  scale_y_discrete(expand = c(0, 0)) +
  # scale_x_continuous(expand = c(0, 0), name = expression(italic(S)[CWDX20]), limits = c(0,1000)) +
  scale_fill_manual(
    values = c("#D55E0050", "#0072B250"), 
    labels = c(expression(italic(S)[CWDX40]), 
               expression(italic(S)[dSIF]))) +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA, name = "")
    )
  ) +
  ggtitle(expression(italic(S)[CWDX40] ~ "and" ~ italic(S)[dSIF])) +
  theme_ridges(center = TRUE) +
  # scale_x_continuous(expand = c(0, 0), name = "Rooting depth (mm)", limits = c(0,1000)) +
  scale_x_log10(name = expression(italic(S)[CWDX40] ~ "and" ~ italic(S)[dSIF] ~ " (mm)"),
                breaks = c(1, 10, 100, 1000),
                expand = c(0, 0),
                limits = c(10, 10000)) +
  labs(y = "")

ggsave("fig/modobs_ridges_cwdx_dsif.pdf", width = 15, height = 10)
ggsave("fig/modobs_ridges_cwdx_dsif.png", width = 15, height = 10)

df_corr |> dplyr::select(biome_name, obs = cwd_lue0_fet, mod = cwdx40) |>
  drop_na() |> 
  tidyr::pivot_longer(cols = c(mod, obs), names_to = "source", values_to = "cwdx") |> 
  dplyr::filter(biome_name!="Mangroves") |>   
  ggplot(
    aes(x = cwdx, y = biome_name, color = source, point_color = source, fill = source)
    ) +
  geom_density_ridges(jittered_points = FALSE) +
  scale_y_discrete(expand = c(0, 0)) +
  # scale_x_continuous(expand = c(0, 0), name = expression(italic(S)[CWDX20]), limits = c(0,1000)) +
  scale_fill_manual(
    values = c("#D55E0050", "#0072B250"), 
    labels = c(expression(italic(S)[CWDX40]), 
               expression(italic(S)[dEF]))) +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA, name = "")
    )
  ) +
  ggtitle(expression(italic(S)[CWDX40] ~ "and" ~ italic(S)[dEF])) +
  theme_ridges(center = TRUE) +
  scale_x_log10(name = expression(italic(S)[CWDX40] ~ "and" ~ italic(S)[dEF] ~ " (mm)"), 
                breaks = c(1, 10, 100, 1000),
                expand = c(0, 0),
                limits = c(10, 10000)) +
  labs(y = "")

ggsave("fig/modobs_ridges_cwdx_fet.pdf", width = 15, height = 10)
ggsave("fig/modobs_ridges_cwdx_fet.png", width = 15, height = 10)

lon_hires <- seq(-179.975, 179.975, by = 0.05)
ilon <- purrr::map_int(as.list(df$lon), ~which.min(abs(. - lon_hires)))

library(ingestr)
library(segmented)
source("R/calc_cwd_et0_byilon.R")

dfs <- siteinfo_fluxnet2015 |> 
  dplyr::select(sitename, lon, lat) |> 
  
  ## get hires longitude index
  mutate(ilon = ilon) |> 
  group_by(ilon) |> 
  nest() |> 
  rename(siteinfo = data) |> 
  
  ungroup() |> 
  slice(1) |> 
  
  mutate(data = purrr::map2(ilon, siteinfo, ~calc_cwd_et0_byilon(.x, siteinfo = .y, drop_data = FALSE, overwrite = TRUE, do_plot = TRUE)))

saveRDS(dfs, file = "data/sample_checks_fluxnet2015sites.rds")

## One plot can then be shown as
dfs$data[[1]]$out_lue0_fet[[1]]$gg + labs(subtitle = dfs$siteinfo[[1]]$sitename)

load("data/gg_fig1a.Rdata")
load("data/gg_fig1c.Rdata")
load("data/gg_fig1b.Rdata")

plot_grid(gg_fig1a, gg_fig1b, gg_fig1_legend, ncol = 1, labels = c('a', 'b', ''), rel_heights = c(1,1,0.2))
ggsave("fig/fig1.png", width = 8, height = 8)
ggsave("fig/fig1.pdf", width = 8, height = 8)

# Fig. 5
load("data/gg_fig5a.Rdata")
load("data/gg_fig5b.Rdata")
bottom_row <- plot_grid(gg_fig5c, gg_fig5d, ncol = 2, labels = c('c', 'd'))
plot_grid(gg_fig4a, gg_fig4b, gg_fig4_legend, bottom_row, ncol = 1, labels = c('a', 'b', ''), rel_heights = c(1,1,0.25,0.7))
ggsave("fig/fig_return_period.pdf", width = 8, height = 11)
ggsave("fig/fig_return_period.png", width = 8, height = 11)

cowplot::plot_grid(gg$ggmap, gg$gglegend, ncol = 2, rel_heights = c(1, 0.12))

ggsave("fig/map_zroot_cwd80.png", width = 10, height = 5)
ggsave("fig/map_zroot_cwd80.pdf", width = 10, height = 5)

## fig 3
# top <- cowplot::plot_grid(gg_fig3a, gg_fig3a_legend, 
#                           gg_fig3b, gg_fig3b_legend, 
#                           ncol = 2, rel_widths = c(1, 0.12), labels = c('a', '', 'b', ''))
# bottom <- cowplot::plot_grid(gg_hist_cwdx80, gg_hist_zr,
#                              ncol = 2, labels = c('c', 'd'))
# cowplot::plot_grid(top, bottom, nrow = 2, rel_heights = c(1, 0.3))

cowplot::plot_grid(gg_fig3a, gg_fig3a_legend, 
                   gg_fig3b, gg_fig3b_legend, 
                   ncol = 2, rel_widths = c(1, 0.08), labels = c('a', '', 'b', ''))
ggsave("fig/fig3.png", width = 10, height = 8)
ggsave("fig/fig3.pdf", width = 10, height = 8)
