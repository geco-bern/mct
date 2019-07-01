# mct

Steps of my analysis:

## MCT method

`mct_method.Rmd`

## ET evaluation

- Run site-scale simulations (done on my laptop)
- Analyse results using `eval_et.Rmd`

## Simulate global optimal rooting depth

- On CX1, run global simulation.
- Calculate optimal rooting zone WHC globally using `rscript_mct_global.R`.
- Create map of optimal WHC using `whc_deficit_global.Rmd`.
- Derive WHC per unit depth using HWSD data and create global maps (NetCDF) using `whc_hwsd.Rmd`.
- Given optimal WHC, calculate corresponding depth, accounting for soil texture, using `zroot_global.Rmd`.
- Analyse performance using `modobs_zroot_ecosystem.Rmd`.