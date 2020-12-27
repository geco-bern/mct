#!/bin/bash

bsub -n 36 -W 72:00 -u bestocke -J collect_cwd_lue0 -R "span[ptile=36]" "R --vanilla --slave < ~/mct/rscript_collect_cwd_lue0.R > ~/hpc_log/rscript_collect_cwd_lue0.Rout"