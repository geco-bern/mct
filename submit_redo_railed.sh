#!/bin/bash

njobs=1
for ((n=1;n<=${njobs};n++)); do
    echo "Submitting chunk number $n ..."
    bsub -W 72:00 -u bestocke -J "redo_failed $n" -R "rusage[mem=25000]" "Rscript --vanilla rscript_redo_failed.R $n $njobs"
done

# #bsub -n 512 -W 72:00 -u bestocke -J "redo_failed $n" -R "span[ptile=128]" "Rscript --vanilla rscript_redo_failed.R $n $njobs"
# 
# bsub -W 72:00 -u bestocke -J "redo_failed $n" -R "rusage[mem=25000]" "Rscript --vanilla rscript_redo_failed.R $n $njobs"