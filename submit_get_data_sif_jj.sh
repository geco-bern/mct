#!/bin/bash

module load netcdf
module load new r/3.6.0

bsub -n 128 -W 72:00 -u bestocke -J get_data_sif_jj -R "span[ptile=128]" "R --vanilla --slave < ~/mct/rscript_get_data_sif_jj.R > ~/hpc_log/rscript_get_data_sif_jj.Rout"