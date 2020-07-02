#!/bin/bash

njobs=100
for ((n=1;n<=${njobs};n++)); do
    echo "Submitting chunk number $n ..."
    bsub -W 72:00 -u bestocke -J "get_bal $n" -R "rusage[mem=50000]" "Rscript --vanilla rscript_get_bal.R $n $njobs"
done