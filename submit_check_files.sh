#!/bin/bash

bsub -W 72:00 -u bestocke -J "check_files" -R "rusage[mem=10000]" "Rscript --vanilla rscript_check_files.R"