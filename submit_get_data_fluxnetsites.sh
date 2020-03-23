#!/bin/bash

bsub -N -o ~/hpc_log/cluster_get_data_fluxnetsites.out  -n 3 -W 12:00 -R "rusage[mem=10000]" "R --vanilla --slave < ~/mct/rscript_get_data_fluxnetsites.R  >~/hpc_log/rscript_get_data_fluxnetsites.Rout"
