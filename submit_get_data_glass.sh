#!/bin/bash

# bsub -n 10 -W 48:00 -u bestocke -J get_data_glass -R "rusage[mem=5000]" "R --vanilla --slave < ~/mct/rscript_get_data_glass.R > ~/hpc_log/rscript_get_data_glass.Rout"

njobs=100
for ((n=1;n<=${njobs};n++)); do
    echo "Submitting chunk number $n ..."
    bsub -W 72:00 -u bestocke -J "get_data_glass $n" -R "rusage[mem=20000]" "Rscript --vanilla rscript_get_data_glass.R $n $njobs"
done