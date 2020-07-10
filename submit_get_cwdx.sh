#!/bin/bash

# njobs=1
# for ((n=1;n<=${njobs};n++)); do
#     echo "Submitting chunk number $n ..."
#     bsub -W 72:00 -u bestocke -J "get_cwdx $n" -R "rusage[mem=25000]" "Rscript --vanilla rscript_get_cwdx.R $n $njobs"
# done

bsub -n 512 -W 72:00 -u bestocke -J "get_cwdx $n" -R "span[ptile=128]" "Rscript --vanilla rscript_get_cwdx.R $n $njobs"