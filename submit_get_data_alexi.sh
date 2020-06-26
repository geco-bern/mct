#!/bin/bash

# bsub -N -o ~/hpc_log/cluster_get_data_alexi.out -W 01:00 -R -n 2 mpirun "rusage[mem=10000]" "R --vanilla --slave < ~/mct/rscript_get_data_alexi.R  >~/hpc_log/rscript_get_data_fluxnetsites.Rout

# bsub -n 512 -W 72:00 -u bestocke -J get_data_alexi -R "span[ptile=128]" "R --vanilla --slave < ~/mct/rscript_get_data_alexi.R > ~/hpc_log/rscript_get_data_alexi.Rout"

njobs=100
for ((n=1;n<=${njobs};n++)); do
    echo "Submitting chunk number $n ..."
    bsub -W 72:00 -u bestocke -J "get_data_alexi $n" -R "rusage[mem=20000]" "Rscript --vanilla rscript_get_data_alexi.R $n $njobs"
done