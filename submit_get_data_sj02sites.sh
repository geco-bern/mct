#!/bin/bash

bsub -N -o ~/hpc_log/cluster_get_data_sj02sites.out -n 1 -W 12:00 -R "rusage[mem=10000]" "R --vanilla --slave < ~/mct/rscript_get_data_sj02sites.R  >~/hpc_log/rscript_get_data_sj02sites.Rout"
