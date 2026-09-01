#!/usr/bin/env bash

if [[ -f ~/.bash_profile ]]; then
  source ~/.bash_profile
fi
set -euo pipefail
project_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
run_id=$(cd "$project_root" && Rscript -e 'source("R/input_config.R"); cat(climate_run_id(read_input_config()))')
cd "$project_root/data"

tagged_name() {
  local filename=$1
  local stem=${filename%.*}
  local extension=${filename##*.}
  printf '%s__%s.%s\n' "$stem" "$run_id" "$extension"
}

eudrs "$(tagged_name df_corr.RData)"
eudrs "$(tagged_name df_rl_fet.RData)"
eudrs "$(tagged_name df_rl_agg_fet.RData)"
eudrs "$(tagged_name df_cwdx_10_20_40.RData)"
eudrs df_whc_hires_lasthope.RData
eudrs df_whc.RData
eudrs "$(tagged_name df_cwd_lue0_2.RData)"
eudrs "$(tagged_name df_cwd_et0_3.RData)"
eudrs df_rivers.RData
eudrs "$(tagged_name df_mct_merged.RData)"
eudrs "$(tagged_name df_mask.RData)"
