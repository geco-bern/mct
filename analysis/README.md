# Analysis sequence

Run scripts from the project root. Directory numbers define the stage order;
numbers within a directory define the normal order inside that stage. The
stages are intentionally restartable checkpoints, not one all-or-nothing
notebook.

## 01 — Tidy inputs

1. `01_watch_prec.R` — configured precipitation
2. `02_watch_snow.R` — configured snowfall
3. `03_watch_swrad.R` — WATCH shortwave radiation
4. `04_watch_temp.R` — configured air temperature
5. `05_alexi.R`, `06_alexi_lores.R` — configured ET at both resolutions
6. `07_sif_jj.R` through `10_sif_pk_lores.R` — both SIF products and resolutions
7. `11_glass.R` — GLASS radiation
8. `12_regrid_evi_max.R` — EVI aggregation
9. `13_extract_global_inputs.R` — remaining global input assembly

Scripts 01–11 call the shared `map2tidy` adapter. They can run independently
where their source NetCDF files exist. The historical script basenames are
retained for continuity, but scripts 01, 02, 04, 05, and 06 read all source
details from `config/input_sources.R` rather than assuming WATCH or ALEXI.

## 02 — Spatial and site preparation

`01_get_site_data.R`, `02_get_fluxnet_data.R`, and `03_get_sj02_data.R` prepare
site inputs. `04_prepare_global_grid.R` and `05_prepare_simsuite.R` prepare the
global/SOFUN configurations. `06_masks_and_metadata.R` holds the global masks
and metadata work extracted from the former workflow notebook.

## 03 — Daily water balance

Run ET conversion and snow simulation first. Then run the high- or
low-resolution balance script. `05_prepare_daily_balance.R` contains the
non-array balance analyses and checks formerly embedded in the notebook.

## 04 — CWD extremes

Run `01_fit_extremes.R`, then `03_extract_return_levels.R`, then
`04_collect_return_levels.R`. `02_fit_extremes_lores.R` is the low-resolution
branch. `05_redo_failed.R` is only for diagnosed incomplete cells.
`06_create_products.R` creates the derived maps and analyses.

## 05 — Soil and rooting depth

Run `01_calculate_soil_parameters.R` as a chunked step and then
`03_combine_soil_parameters.R`. The historical duplicate second calculation
script was removed. `04_calculate_rooting_depth.R` contains the analysis-level
rooting-depth calculations.

## 06–07 — Thresholds and return periods

Each threshold calculation (`01`, `03`) is followed by its corresponding
collector (`02`, `04`). The two analysis scripts (`05`, `06`) use the collected
objects. Return-level scripts `01` and `02` may run in parallel, after which
`03` collects them; `04` diagnoses return periods and `05` collects those
diagnoses. `06` performs the final analysis.

## 08–10 — Sites, diagnostics, and results

Stage 08 prepares RSIP forcing, runs SOFUN chunks, and evaluates FLUXNET. Stage
09 checks output completeness and selected sites. Stage 10 creates the final
bias analysis and figures.

## Chunk controls

Chunked scripts accept `CHUNK TOTAL_CHUNKS`. `MCT_ILON` restricts a run to exact
longitude indices. Existing per-longitude products are skipped, allowing safe
restart after interruption. All run-dependent paths include the ET and
precipitation IDs selected in `config/input_sources.R`. Run
`Rscript analysis/00_status.R` to compare the current configured run's output
count with the UBELIX job registry.
