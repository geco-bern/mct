#!/bin/bash

njobs=100
for ((n=1;n<=${njobs};n++)); do
    echo "Submitting chunk number $n ..."
    bsub -W 72:00 -u bestocke -J "get_data_glass $n" -R "rusage[mem=20000]" "Rscript --vanilla rscript_get_data_glass.R $n $njobs"
done

# bsub -W 72:00 -u bestocke -J "get_data_glass_TEST" -R "rusage[mem=20000]" "Rscript --vanilla rscript_get_data_glass.R 1000 7200"