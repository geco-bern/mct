#!/usr/bin/env bash

set -euo pipefail

submit_job() {
  local job=$1
  local dependency=${2:-}
  if [[ -n "$dependency" ]]; then
    MCT_DEPENDENCY="afterok:${dependency}" src/ubelix/submit.sh "$job"
  else
    src/ubelix/submit.sh "$job"
  fi
}

precipitation_id=$(submit_job prepare_precipitation)
snowfall_id=$(submit_job prepare_snowfall)
watch_swrad_id=$(submit_job watch_swrad)
temperature_id=$(submit_job prepare_temperature)
configured_et_id=$(submit_job prepare_et)
sif_jj_id=$(submit_job sif_jj)
sif_pk_id=$(submit_job sif_pk)
glass_id=$(submit_job glass)

et_id=$(submit_job convert_et_mm "${configured_et_id}:${temperature_id}")
snow_id=$(submit_job simulate_snow "${precipitation_id}:${snowfall_id}:${temperature_id}")
balance_id=$(submit_job calculate_balance "${et_id}:${snow_id}")
cwdx_id=$(submit_job fit_extremes "$balance_id")
extract_id=$(submit_job extract_return_levels "$cwdx_id")
collect_cwdx_id=$(submit_job collect_return_levels "$extract_id")

sif_threshold_id=$(submit_job calculate_sif_thresholds "${cwdx_id}:${sif_jj_id}:${sif_pk_id}:${watch_swrad_id}")
et_threshold_id=$(submit_job calculate_et_thresholds "${cwdx_id}:${configured_et_id}:${glass_id}")
submit_job collect_sif_thresholds "$sif_threshold_id" >/dev/null
submit_job collect_et_thresholds "$et_threshold_id" >/dev/null

echo "Submitted through collected CWDX job ${collect_cwdx_id}."
