#!/bin/bash

bsub -n 36 -W 72:00 -u bestocke -J collect_cwdx -R "span[ptile=36]" "R --vanilla --slave < ~/mct/rscript_collect_cwdx.R > ~/hpc_log/rscript_collect_cwdx.Rout"