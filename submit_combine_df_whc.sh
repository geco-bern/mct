#!/bin/bash
  
bsub -W 12:00 -u bestocke -J combine_df_whc -R "rusage[mem=100000]" "Rscript --vanilla rscript_combine_df_whc.R"
