#!/bin/bash

njobs=10
for ((n=1;n<=${njobs};n++)); do
    echo "Submitting chunk number $n ..."
    bsub -W 72:00 -u bestocke -J "calc_soilparams $n" -R "rusage[mem=2000]" "Rscript --vanilla rscript_calc_soilparams.R $n $njobs"
done