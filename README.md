# mct

*Benjamin D. Stocker*

Code for the analysis published as *Global patterns of water storage in the
rooting zones of vegetation*. The repository name derives from the Mass Curve
Technique used by [Gao et al. (2014)](https://doi.org/10.1002/2014GL061668).
The code is licensed under GPL-3.

## Repository layout

The project follows the structure of a reproducible R analysis:

- `analysis/` contains numbered, executable work steps. Run scripts from the
  project root and in numerical order.
- `config/` contains the user-editable climate-input namelist.
- `R/` contains reusable functions and no analysis orchestration.
- `data-raw/` contains small, immutable source material kept with the project,
  currently the CDO grid definitions.
- `data/` is the location of generated and downloaded analysis data. Existing
  filenames and saved R object names are part of the workflow contract.
- `fig/` contains generated figures.
- `vignettes/` contains focused reports. The former monolithic workflow is
  retained only as `vignettes/archive/workflow_legacy.Rmd` for provenance.
- `src/` contains shell utilities, data-transfer helpers, and the UBELIX Slurm
  submission layer.
- `tests/` contains focused tests for workflow infrastructure.

Generated files under `data/`, `fig/`, and `logs/` are ignored by Git.
Run-dependent output names include their configured ET and precipitation
identifiers, so products from alternative input combinations can coexist.

## Software setup

Use R 4.1 or newer and run all commands from the directory containing
`mct.Rproj`. The core workflow uses the tidyverse and geospatial/statistical
packages recorded in `DESCRIPTION`. In a fresh R installation:

```r
install.packages(c("remotes", "renv"))
renv::init()
renv::snapshot()
```

`map2tidy`, `rbeni`, `ingestr`, and `rsofun` are recorded as GitHub remotes in
`DESCRIPTION`. If installing them individually is preferable:

```r
remotes::install_github("geco-bern/map2tidy")
remotes::install_github("geco-bern/rbeni")
remotes::install_github("geco-bern/ingestr")
remotes::install_github("geco-bern/rsofun")
```

Several later scripts also call command-line geospatial software, principally
CDO, NCO, and GDAL. Historical reports may require retired packages such as
`rgdal`; those reports are not prerequisites for the computational pipeline.

## Climate-input namelist

Edit [`config/input_sources.R`](config/input_sources.R) to select the ET,
precipitation, and temperature datasets. It is a plain R list so it can be read
without an extra configuration package, and serves the same purpose as a
FORTRAN `NAMELIST`. Each source entry defines:

- a short `id` for the dataset;
- the NetCDF directory and filename pattern;
- the tidy-cache directory and filename prefix;
- NetCDF variable and coordinate names;
- scale and offset transformations;
- the longitude/latitude grid.

ET sources additionally set `conversion` to either
`"latent_energy_to_mm"` (energy converted with configured temperature and
elevation) or `"identity_mm_day"` (the source is already ET in mm d-1). The
rain, snow, temperature, and low-resolution ET grids must match because those
inputs are joined during snow simulation and the low-resolution balance. The
default namelist reproduces the existing ALEXI ET and WATCH-WFDEI
precipitation/temperature analysis for 2003–2017.

The selected ET and precipitation IDs form a run ID such as
`et-alexi__prec-watch-wfdei`. Every climate-dependent checkpoint, NetCDF,
diagnostic, figure, and UBELIX log carries that suffix, for example:

```text
data/df_bal/df_bal_ilon_123__et-alexi__prec-watch-wfdei.RData
fig/map_cwdx80__et-alexi__prec-watch-wfdei.pdf
```

To maintain multiple namelists, copy the default file and select one without
editing code:

```sh
MCT_INPUT_CONFIG=config/input_sources_era5.R \
  Rscript analysis/03_water_balance/01_convert_et_mm.R 1 100
```

The same environment variable is inherited by UBELIX jobs and the pipeline
submitter. `R/input_config.R` validates the namelist before computation and
provides the common grid and naming functions; analysis variable names remain
unchanged.

## Other external inputs

Large immutable inputs remain outside the repository. Climate paths are set in
the namelist above. Other established `~/data/...` paths can be reproduced on
another system with links or mounts. The remaining machine-specific controls
are:

| Variable | Purpose | Default |
|---|---|---|
| `MCT_PYTHON` | Python used by the Google Earth Engine helper | `python3` on `PATH` |
| `MCT_SOFUN_OUTPUT` | Global SOFUN NetCDF output directory | `~/sofun/output_nc_global_sofun` |
| `MCT_ROOT_PROFILES` | Schenk & Jackson root-profile CSV | its established path under `~/data` |
| `MCT_MODIS_EVI_DIR` | MODIS EVI source directory for the shell regridding utility | required by that utility |

## Work steps

The numbered directories are checkpoints. Each computationally heavy step
writes one file per longitude or job-array chunk; collectors run only after all
required chunks are present.

| Stage | Purpose | Main checkpoint |
|---|---|---|
| `01_tidy_inputs` | Convert configured gridded NetCDF inputs to nested tidy R data using `map2tidy` | tagged `*_ilon_*__et-*__prec-*.RData` files in configured `data_tidy` directories |
| `02_prepare_spatial` | Prepare site metadata, masks, grids, and spatial inputs | site/grid objects in `data/` |
| `03_water_balance` | Convert ET, simulate snow, and calculate daily balance | `data/df_bal/df_bal_ilon_*__et-*__prec-*.RData` |
| `04_cwd_extremes` | Fit cumulative-water-deficit extremes and collect return levels | tagged `data/df_cwdx_10_20_40*.RData` |
| `05_soil` | Calculate and combine soil hydraulic properties, then rooting depth | `df_whc_hires_ilon_*.RData` and combined WHC objects |
| `06_thresholds` | Diagnose SIF- and ET-based CWD thresholds | tagged `data/df_cwd_lue0_2*.RData`, `data/df_cwd_et0_3*.RData` |
| `07_return_periods` | Calculate, diagnose, and collect return periods | tagged `data/df_rl_*.RData`, `data/df_rp_diag_*.RData` |
| `08_site_analysis` | Run RSIP/SOFUN and FLUXNET site analyses | chunked forcing and SOFUN output under `data/` |
| `09_diagnostics` | Check completeness and selected site results | file-availability and diagnostic objects |
| `10_results` | Create bias analyses and publication figures | figures and final derived objects |

`analysis/README.md` gives the detailed script order. Check progress at any time:

```sh
Rscript analysis/00_status.R
```

## Running and restarting locally

Small steps can be run directly:

```sh
Rscript analysis/02_prepare_spatial/02_get_fluxnet_data.R
```

Chunked steps accept `CHUNK TOTAL_CHUNKS`. For example, this runs the third of
100 disjoint longitude groups:

```sh
Rscript analysis/03_water_balance/01_convert_et_mm.R 3 100
```

Set `MCT_ILON` to diagnose or recompute selected longitude indices without
editing a script. It accepts comma-separated values and inclusive ranges:

```sh
MCT_ILON=3961-3970,5000 Rscript analysis/04_cwd_extremes/01_fit_extremes.R
```

Computational functions skip outputs that already exist. Consequently, the
same command safely resumes an interrupted stage and its messages identify
completed and missing longitude bands. The NetCDF conversion adapter also
writes through a temporary file before exposing each completed `.RData` file.
Remove or relocate only the exact corrupt output that should be regenerated;
do not clear a whole checkpoint directory.

Parallel work is bounded by `SLURM_CPUS_PER_TASK` on UBELIX and by locally
available physical cores elsewhere. This avoids nested clusters and CPU
oversubscription. The largest row-wise soil extraction was changed to bounded
batches so it does not create one task per grid cell.

## NetCDF-to-tidy conversion

Scripts `analysis/01_tidy_inputs/01_*.R` through `11_*.R` are declarative
wrappers around `R/map_netcdf_to_tidy.R`. The adapter delegates NetCDF reading
and longitude chunking to `map2tidy`, then restores the established downstream
contract:

- output basenames retain their source prefix and add the run ID, for example,
  `EDAY_CERES__ilon_123__et-alexi__prec-watch-wfdei.RData`;
- every file still contains an object named `df`;
- the outer columns remain `lon`, `lat`, and nested `data`;
- nested `datetime` is normalised to the existing `time` name.

This isolates the generic conversion from source-specific variable and path
configuration without changing analysis variable names.

## UBELIX

The old LSF `bsub` submission scripts have been replaced by Slurm-compatible
UBELIX scripts in `src/ubelix/`. The job registry records script paths, arrays,
CPU, memory, time, and expected output counts.

```sh
src/ubelix/submit.sh prepare_et
src/ubelix/submit.sh calculate_balance
src/ubelix/submit_pipeline.sh
```

Jobs use `%A_%a` logs under `logs/ubelix/`, with the run ID included in each log
name, `SLURM_ARRAY_TASK_ID` for arrays, `afterok` dependencies for the pipeline,
`srun` for the R process, and the R environment requested through UBELIX
modules. See `src/ubelix/README.md` for account, partition, Workspace, resource,
and recovery controls.

## Reusable scientific code

The principal reusable functions remain under `R/`:

- `convert_et.R`: ET conversion to mass units;
- `simulate_snow2.R`: snow mass balance;
- `mct2.R`: cumulative water balance;
- `get_plantwhc_mct_bysite.R`: extreme-value fitting;
- `calc_soilparams.R`: soil water-holding capacity;
- `calc_cwd_lue0_v2.R`: SIF/EF threshold diagnosis;
- `workflow_helpers.R`: paths, chunking, allocation-aware parallelism, and
  atomic checkpoint writes;
- `input_config.R`: climate-input validation, grid lookup, unit transforms,
  and run-specific filenames;
- `map_netcdf_to_tidy.R`: generic `map2tidy` compatibility adapter.

Analysis-specific choices, paths, object assembly, and plotting remain in the
numbered scripts rather than being hidden in reusable functions.

## Focused reports

The R Markdown files under `vignettes/` are supplementary analyses and reports,
not the orchestration layer. Each active report sets its knit root to the
project directory. See `vignettes/README.md` for the report index and the role
of the archived workflow notebook.
