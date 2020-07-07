#!/bin/bash

bsub -n 256 -W 72:00 -u bestocke -J extract_cwdx -R "span[ptile=128]" "R --vanilla --slave < ~/mct/rscript_extract_cwdx.R > ~/hpc_log/rscript_extract_cwdx.Rout"