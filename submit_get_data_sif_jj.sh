#!/bin/bash

bsub -n 2 -W 72:00 -u bestocke -J get_data_sif_jj -R "rusage[mem=5000]" "R --vanilla --slave < ~/mct/rscript_get_data_sif_jj.R > ~/hpc_log/rscript_get_data_sif_jj.Rout"