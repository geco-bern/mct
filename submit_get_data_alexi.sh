#!/bin/bash

bsub -N -o ~/hpc_log/cluster_get_data_alexi.out -n 2 -W 01:00 -R "rusage[mem=10000]" "R --vanilla --slave < ~/mct/rscript_get_data_alexi.R  >~/hpc_log/rscript_get_data_alexi.Rout"
