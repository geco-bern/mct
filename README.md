# mct

*Benjamin D. Stocker*

Code for analysis published as *Global distribution of the rooting zone water storage capacity reflects plant adaptation to the environment*.

Code of this repository is published under a GPL v3 license.

## Important files

- `workflow.Rmd`: Outlines all steps for the global analysis. Implements plotting of published figures. Computation-heavy step are implemented for cluster computing (using files `rscript_*.R` and `submit_*.sh`).
- `rsip.Rmd`: Implements all steps for comparison of global rooting depth estimates against observations, and additional analyses of observations (trait gradient analysis, etc.)
- `eval_et.Rmd`: Implements the evaluation of different remote sensing-based ET products against observations from FLUXNET.
- `rsofun_rsip_mct.Rmd`: Additional rsofun (SPLASH) simulations at RSIP site locations and visualisations, to test method for diagnosing S0.
- `create_suppl_info.Rmd`: Additional visualisations of method for diagnosing S0. 

## Key analysis steps:

- ET conversion to mass units: `R/convert_et.R`
- Snow mass balance: `R/simulate_snow2.R`
- Cumulative water balance algorithm: `R/mct2.R` (same as implemented by the [*cwd* R package](https://zenodo.org/record/5359053#.YUWKMJ4zY8M))
- Extreme value distribution fitting: `R/get_plantwhc_mct_bysite.R`
- Soil water holding capacity using pedo-transfer functions: `R/calc_soilparams.R`
- Diagnosing $S_0$ and "flattening" relationships from SIF and EF vs. CWD: `R/calc_cwd_lue0_v2.R`