#!/bin/bash

njobs=30
for ((n=1;n<=${njobs};n++)); do
    echo "Submitting chunk number $n ..."
    bsub -W 72:00 -u bestocke -J "get_data_sif_jj $n" -R "rusage[mem=5000]" "Rscript --vanilla rscript_get_data_sif_jj.R $n $njobs"
done