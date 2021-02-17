#!/bin/bash
  
#bsub -n 128 -W 72:00 -u bestocke -J combine_df_whc_from_save -R "span[ptile=128]" "Rscript --vanilla rscript_combine_df_whc.R"
bsub -W 72:00 -u bestocke -J combine_df_whc_lasthope -R "rusage[mem=12000]" "Rscript --vanilla rscript_combine_df_whc.R"