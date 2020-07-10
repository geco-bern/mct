#!/bin/bash

#bsub -n 2 -W 12:00 -u bestocke -J simulate_snow -R "rusage[mem=10000]" "R --vanilla --slave < ~/mct/rscript_simulate_snow.R > ~/hpc_log/rscript_simulate_snow.Rout"

njobs=1
for ((n=1;n<=${njobs};n++)); do
    echo "Submitting chunk number $n ..."
    bsub -W 72:00 -u bestocke -J "simulate_snow $n" -R "rusage[mem=10000]" "Rscript --vanilla rscript_simulate_snow.R $n $njobs"
done