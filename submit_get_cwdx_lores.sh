#!/bin/bash

bsub -W 72:00 -u bestocke -J "get_cwdx_lores 681" -R "rusage[mem=48000]" "Rscript --vanilla rscript_get_cwdx_lores.R"
