#!/usr/bin/env bash

set -euo pipefail

job_name=${1:-}
if [[ -z "$job_name" ]]; then
  echo "Usage: src/ubelix/submit.sh JOB_NAME" >&2
  echo "Available jobs:" >&2
  tail -n +2 src/ubelix/jobs.tsv | cut -f2 >&2
  exit 2
fi

row=$(awk -F '\t' -v name="$job_name" '$2 == name { print; exit }' src/ubelix/jobs.tsv)
if [[ -z "$row" ]]; then
  echo "Unknown job: $job_name" >&2
  exit 2
fi

IFS=$'\t' read -r stage job script mode array chunks max_parallel cpus memory walltime output_pattern expected_outputs extra_args <<< "$row"

mkdir -p logs/ubelix
account=${MCT_SLURM_ACCOUNT:-gratis}
dependency=${MCT_DEPENDENCY:-}
if ! command -v Rscript >/dev/null 2>&1; then
  module load R
fi
run_id=$(Rscript -e 'source("R/input_config.R"); cat(climate_run_id(read_input_config()))')

options=(
  --parsable
  --account="$account"
  --job-name="mct_${job}_${run_id}"
  --time="$walltime"
  --ntasks=1
  --cpus-per-task="$cpus"
  --mem="$memory"
  --output="logs/ubelix/${job}__${run_id}_%A_%a.out"
  --error="logs/ubelix/${job}__${run_id}_%A_%a.err"
)

if (( array > 1 )); then
  options+=(--array="1-${array}%${max_parallel}")
fi
if [[ -n "$dependency" ]]; then
  options+=(--dependency="$dependency")
fi
if [[ -n ${MCT_SLURM_PARTITION:-} ]]; then
  options+=(--partition="$MCT_SLURM_PARTITION")
fi

sbatch "${options[@]}" src/ubelix/run_r_job.sh "$script" "$mode" "$chunks" "$extra_args"
