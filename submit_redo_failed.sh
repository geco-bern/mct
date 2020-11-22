#!/bin/bash

njobs=100
for ((n=1;n<=${njobs};n++)); do
    echo "Submitting chunk number $n ..."
    bsub -W 72:00 -u bestocke -J "redo_failed $n" -R "rusage[mem=20000]" "Rscript --vanilla rscript_redo_failed.R $n $njobs"
done

# bsub -W 72:00 -u bestocke -J "redo_failed_TESTS" -R "rusage[mem=20000]" "Rscript --vanilla rscript_redo_failed.R 1153 7200"