#!/bin/bash

bsub -W 72:00 -u bestocke -J "rp_diag" -R "rusage[mem=4800]" "Rscript --vanilla rscript_rp_diag.R"
