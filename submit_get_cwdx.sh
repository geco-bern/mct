#!/bin/bash

# njobs=255
# for ((n=1;n<=${njobs};n++)); do
#     echo "Submitting chunk number $n ..."
#     bsub -W 72:00 -u bestocke -J "get_cwdx $n" -R "rusage[mem=10000]" "Rscript --vanilla rscript_get_cwdx.R $n $njobs"
# done

bsub -W 72:00 -u bestocke -J "get_cwdx 681" -R "rusage[mem=48000]" "Rscript --vanilla rscript_get_cwdx.R 681 7200"

# bsub -n 512 -W 72:00 -u bestocke -J "get_cwdx $n" -R "span[ptile=128]" "Rscript --vanilla rscript_get_cwdx.R $n $njobs"