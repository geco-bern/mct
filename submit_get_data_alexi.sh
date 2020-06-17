#!/bin/bash

# bsub -N -o ~/hpc_log/cluster_get_data_alexi.out -W 01:00 -R -n 2 mpirun "rusage[mem=10000]" "R --vanilla --slave < ~/mct/rscript_get_data_alexi.R  >~/hpc_log/rscript_get_data_fluxnetsites.Rout

bsub -n 10 -W 48:00 -u bestocke -J get_data_alexi -R "rusage[mem=5000]" "R --vanilla --slave < ~/mct/rscript_get_data_alexi.R > ~/hpc_log/rscript_get_data_alexi.Rout"
