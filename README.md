# mct

## Approach

- **Hypothesis 1**: Plants should be acclimated to the maximum cumulative water deficit with a return period of *N* years (CWD*N).
- **Test data 1:** Reliability of ALEXI and WATCH-WFDEI for estimating CWD, data: FLUXNET. ALEXI captures ET during dry periods → Use this in combination with precipitation estimates to estimate the apparent rooting zone water storage capacity.
- **Test H1:** CWD*N ~ zroot95, data: SJ02 → CWD*N governs maximum rooting depth variations across the globe (across biomes). Do a similar analysis with Shersingh's data.
- **Global application 1:**
    - Map of CWD*N and implied (estimated) rooting depth (accounting for soil texture).
    - CWD'N ~ (aridity index **x** seasonality index) → largest rooting depth at intermediate aridity with pronounced seasonality.
- **Challenge**: Rooting depth measurements are sparse. → Is there a way to "sense" rooting depth from above?
- **Hypothesis 2:** CWD*N to which rooting depth is acclimated should also be reflected by the CWD at which LUE drops to zero. → Alternative approach to estimate CWD*N, and CWD*N determines sensitivity to drought.
- **Test H2:** CWD'N ~ CWD_LUE0 (CWD at which LUE is zero), data: FLUXNET sites → Analyze correlation of CWD'N ~ CWD_LUE0.
- **Test data 2**: Reliability of SiF data for estimating GPP, and SiF/PAR and ALEXI/WATCH-WFDEI -based CWD'N ~ CWD_LUE0 analysis.
- **Global application 2:**
    - Map of CWD_LUE0
    - Same pattern across globe? biomes? (aridity index **x** seasonality index)-space?
- (Hopefully) **conclusion**: Rooting depth is acclimated to the hydro-climate and determines the global distribution of drought impact thresholds on vegetation productivity.


Steps of my analysis:

## Steps

### Evaluation at FLUXNET sites

1. Extract ET and precip time series data from global files at FLUXNET sites locations (LandEval and WATCH) `rscript_get_data_fluxnetsites.R`
    - [ ]  LandEval data: re-do it, extracting all years' data from 1984 - 2007 (submitted 23.3.2020)
    - [x]  ALEXI data (submitted 23.3.2020)
2. Evaluate ET from LandEval during rain-free periods against FLUXNET data `rscript_eval_fluxnet2015.R`
    - [ ]  LandEval data
    - [ ]  ALEXI data
3. Apply MCT method to identify CWD events based on SEBS and precipitation from FLUXNET or WATCH (should yield a list of events for each site)
    - [ ]  Assess CWD time series: linear trend? Plausible end-of-event
4. Extract SiF and SiF/APAR time series data from global files at FLUXNET sites locations (time series) `not implemented`
    - [ ]  SiF time series
    - [ ]  MODIS FPAR time series
    - [ ]  WATCH-WFDEI SWdown time series
5. Derive k (slope or decay rate of SiF (and SiF/APAR) and observed GPP (and LUE) vs. CWD and d) at FLUXNET sites locations; do with normalising SiF with APAR or PAR
6. Estimate rooting depth from CWD statistics (MCT method) → zroot*
7. Relate k to zroot*

### Ecosystem-level zroot

Do steps 1, 3, 4, 5, and 6 for sites in **Schenk & Jackson data**

Evaluation aggregated by biome

- Observed 95% ile rooting depth
- MCT-estimated rooting depth
- k_SiF

### Spatial bins zroot

Do steps 1, 3, 4, 5, and 6 for sites in **Shersingh's data**

Evaluation aggregated by biome or spatial bins (~1-2 deg)

- Observed 95% ile rooting depth
- MCT-estimated rooting depth
- k_SiF

### Application at global 0.5 deg

Do steps 1, 3, 4, 5, and 6 for each pixel globally.

Visualisation of global maps

- For datasets that are available at 0.05 deg, aggregate to 0.5 deg masking out non-forest pixels at high resolution, and area-weighted mean across remaining hi-res gridcells within lo-res gridcell. Use MODIS land cover data to determine forest cover



--- Below is old:

## MCT method

`mct_method.Rmd`

## Method evaluation

1. Get ET and precip data for FLUXNET sites and extract from global files (WATCH-WFDEI and LandFlux) at respective sites for comparison: `rscript_get_data_fluxnetsites.R`
2. Evaluate ET during rain-free periods `rscript_eval_fluxnet2015.R`: Creating file `data/df_eval.csv` and with aligned data `data/df_alg__df_dday_aggbydday.Rdata`. This is used to evaluate whether ET products can accurately simulate ET during dry rain-free periods (i.e., whether bias w.r.t. EC measurements shows any relationship to the duration of rain-free periods).
3. Apply MCT method to identify CWD events based on SEBS and precipitation from FLUXNET or WATCH (should yield a list of events for each site)
4. Get SiF data for FLUXNET sites (time series)

**Old:**
- `calibrate_ET.Rmd` replaces old `calib_mct_fluxnet2015.Rmd`
- Run vignette 'splash_fluxnet2015.Rmd' in rsofun repo to prepare input files for FLUXNET 2015 simulations (for PET)
>>>>>>> 1da34a5d0ceb62d52c33958539581f52233704e3
- Run site-scale simulations (done on my laptop)
- Analyse results using `eval_et.Rmd`


## MCT evaluation

## Simulate global optimal rooting depth

- On CX1, run global simulation.
- Calculate optimal rooting zone WHC globally using `rscript_mct_global.R`.
- Create map of *optimal WHC* using `whc_deficit_global.Rmd`.
- Derive WHC per unit depth using HWSD data and create global maps (NetCDF) using `whc_hwsd.Rmd`.
- Given *optimal WHC*, calculate corresponding depth, accounting for soil texture, using `zroot_global.Rmd`.
- Analyse performance using `modobs_zroot_ecosystem.Rmd`.

## Simulate optimal rooting depth at Schenk & Jackson 2002 sites

- (On CX1, run global simulation)
- Calculate optimal rooting zone WHC for each site using `rscript_mct_simsuite.R`.
- Do analyses of modelled versus observed using `mct_sj02_simsuite.Rmd`.

## Other analyses

- EVI as fAPAR: `fapar_max_global.Rmd`

## Analyse RSIP data

- Analyse global rooting depth distribution from the RSIP data (Shersing's individual-level data): `rsip.Rmd`



