#!/bin/bash

njobs=7200
for ((n=1;n<=${njobs};n++)); do
    echo "Submitting chunk number $n ..."
    bsub -W 72:00 -u bestocke -J "get_et_mm $n" -R "rusage[mem=25000]" "Rscript --vanilla rscript_get_et_mm.R $n $njobs"
done