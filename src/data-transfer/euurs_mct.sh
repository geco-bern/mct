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

euurs "$(tagged_name df_corr.RData)"
euurs "$(tagged_name gg_fig1a.Rdata)"
euurs "$(tagged_name gg_fig1b.Rdata)"
euurs "$(tagged_name gg_fig1_legend.Rdata)"
euurs 80.RData
euurs gg_rsip_25.RData
euurs gg_rsip_50.RData
euurs gg_rsip_75.RData
euurs gg_rsip_90.RData
euurs "$(tagged_name gg_sj02_10.RData)"
euurs "$(tagged_name gg_sj02_25.RData)"
euurs "$(tagged_name gg_sj02_50.RData)"
euurs "$(tagged_name gg_sj02_75.RData)"
euurs "$(tagged_name gg_sj02_90.RData)"
euurs "$(tagged_name gg_sif_50.RData)"
euurs "$(tagged_name gg_ef_50.RData)"
euurs "$(tagged_name df_rl_fet.RData)"
euurs "$(tagged_name df_rl_agg_fet.RData)"
euurs "$(tagged_name df_cwdx_10_20_40.RData)"
euurs df_whc_hires_lasthope.RData
euurs df_whc.RData
euurs "$(tagged_name df_cwd_lue0_2.RData)"
euurs "$(tagged_name df_cwd_et0_3.RData)"
euurs df_rivers.RData
euurs "$(tagged_name df_sj02_biome_wwf_NEW.Rdata)"
euurs siteinfo_sj02.RData
euurs "$(tagged_name df_zroot_sj02.RData)"
euurs "$(tagged_name df_mct_merged.RData)"
euurs "$(tagged_name df_mask.RData)"
euurs "$(tagged_name df_zroot80.RData)"
euurs "$(tagged_name cwdx10_tenthdeg.nc)"
euurs "$(tagged_name cwdx20_tenthdeg.nc)"
euurs "$(tagged_name cwdx40_tenthdeg.nc)"
euurs "$(tagged_name cwdx80_tenthdeg.nc)"
euurs "$(tagged_name cwdx100_tenthdeg.nc)"
euurs "$(tagged_name cwdx200_tenthdeg.nc)"
euurs "$(tagged_name gg_fig1c.Rdata)"
