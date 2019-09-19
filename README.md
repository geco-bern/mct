# mct

Steps of my analysis:

## MCT method

`mct_method.Rmd`

## ET evaluation

- Run vignette 'splash_fluxnet2015.Rmd' in rsofun repo to prepare input files for FLUXNET 2015 simulations (for PET)
- Run site-scale simulations (done on my laptop)
- Analyse results using `eval_et.Rmd`

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



