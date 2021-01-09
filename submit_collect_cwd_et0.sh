#!/bin/bash

bsub -W 72:00 -u bestocke -J collect_cwd_et0 -R "rusage[mem=48000]" "R --vanilla --slave < ~/mct/rscript_collect_cwd_et0.R > ~/hpc_log/rscript_collect_cwd_et0.Rout"