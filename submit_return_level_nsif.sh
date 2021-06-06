#!/bin/bash

njobs=30
for ((n=1;n<=${njobs};n++)); do
    echo "Submitting chunk number $n ..."
    bsub -W 72:00 -u bestocke -J "rl nsif $n" -R "rusage[mem=48000]" "Rscript --vanilla rscript_return_level_nsif.R $n $njobs"
done