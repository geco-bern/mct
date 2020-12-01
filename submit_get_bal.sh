#!/bin/bash


# most run through with 25 GB. Some crashed because of memory usage. Increasing to 35 GB and running again... Still crashes (143). Running with 75 GB... 

njobs=100
for ((n=1;n<=${njobs};n++)); do
    echo "Submitting chunk number $n ..."
    bsub -W 72:00 -u bestocke -J "get_bal $n" -R "rusage[mem=75000]" "Rscript --vanilla rscript_get_bal.R $n $njobs"
done