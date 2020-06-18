#!/bin/bash

bsub -n 10 -W 48:00 -u bestocke -J get_data_glass -R "rusage[mem=5000]" "R --vanilla --slave < ~/mct/rscript_get_data_glass.R > ~/hpc_log/rscript_get_data_glass.Rout"

