#!/bin/bash

# bsub -n 256 -W 72:00 -u bestocke -J extract_cwdx -R "span[ptile=128]" "R --vanilla --slave < ~/mct/rscript_extract_cwdx.R > ~/hpc_log/rscript_extract_cwdx.Rout"

# njobs=100
# for ((n=1;n<=${njobs};n++)); do
#     echo "Submitting chunk number $n ..."
#     bsub -W 72:00 -u bestocke -J "extract_cwdx $n" -R "rusage[mem=96000]" "Rscript --vanilla rscript_extract_cwdx.R $n $njobs"   # not all require that much memory
# done

bsub -W 72:00 -u bestocke -J "extract_cwdx 79" -R "rusage[mem=96000]" "Rscript --vanilla rscript_extract_cwdx.R 79 100"   # not all require that much memory