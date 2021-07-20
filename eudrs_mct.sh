#!/bin/bash         

source ~/.bash_profile 

cd data

eudrs df_corr.RData
eudrs df_rl_fet.RData
eudrs df_rl_agg_fet.RData
eudrs df_cwdx_10_20_40.RData
eudrs df_cwdx_10_20_80.RData
eudrs df_whc_hires_lasthope.RData
eudrs df_whc.RData
eudrs df_cwd_lue0_2.RData
eudrs df_cwd_et0_3.RData
eudrs df_rivers.RData
eudrs df_mct_merged.RData
eudrs df_mask.RData

cd ..