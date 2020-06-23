#!/bin/bash

bsub -n 2 -W 12:00 -u bestocke -J simulate_snow -R "rusage[mem=2000]" "R --vanilla --slave < ~/mct/rscript_simulate_snow.R > ~/hpc_log/rscript_simulate_snow.Rout"