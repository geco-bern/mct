#!/bin/bash

module load netcdf
module load new r/3.6.0

# bsub -n 128 -W 72:00 -u bestocke -J get_data_sif_pk -R "span[ptile=128]" "R --vanilla --slave < ~/mct/rscript_get_data_sif_pk.R > ~/hpc_log/rscript_get_data_sif_pk.Rout"

njobs=100
for ((n=1;n<=${njobs};n++)); do
    echo "Submitting chunk number $n ..."
    bsub -W 72:00 -u bestocke -J "get_data_sif_pk $n" -R "rusage[mem=50000]" "Rscript --vanilla get_data_sif_pk.R $n $njobs"
done