#!/bin/bash

# requires large memory, finally set to 96 GB

njobs=100
for ((n=1;n<=${njobs};n++)); do
    echo "Submitting chunk number $n ..."
    bsub -W 72:00 -u bestocke -J "calc_cwd_lue0 $n" -R "rusage[mem=96000]" "Rscript --vanilla rscript_calc_cwd_lue0.R $n $njobs"
done

# ## test
# bsub -W 72:00 -u bestocke -J "calc_cwd_lue0 74" -R "rusage[mem=96000]" "Rscript --vanilla rscript_calc_cwd_lue0.R 74 100"
