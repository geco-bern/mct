#!/bin/bash

# bsub -n 256 -W 72:00 -u bestocke -J extract_cwdx -R "span[ptile=128]" "R --vanilla --slave < ~/mct/rscript_extract_cwdx.R > ~/hpc_log/rscript_extract_cwdx.Rout"

njobs=50
for ((n=1;n<=${njobs};n++)); do
    echo "Submitting chunk number $n ..."
    bsub -W 72:00 -u bestocke -J "extract_cwdx $n" -R "rusage[mem=48000]" "Rscript --vanilla rscript_extract_cwdx.R $n $njobs"   # not all require that much memory
done