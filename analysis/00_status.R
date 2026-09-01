#!/usr/bin/env Rscript

source(here::here("R", "workflow_helpers.R"))

jobs <- read.delim(
  project_path("src", "ubelix", "jobs.tsv"),
  sep = "\t",
  quote = "",
  comment.char = "",
  stringsAsFactors = FALSE
)

count_outputs <- function(pattern) {
  if (!nzchar(pattern) || pattern == "-") return(NA_integer_)
  patterns <- strsplit(pattern, ";", fixed = TRUE)[[1]]
  sum(vapply(patterns, function(item) {
    length(Sys.glob(path.expand(item)))
  }, integer(1)))
}

jobs$outputs_found <- vapply(jobs$output_pattern, count_outputs, integer(1))
jobs$status <- ifelse(
  is.na(jobs$expected_outputs),
  ifelse(jobs$outputs_found > 0L, "started", "not started"),
  ifelse(jobs$outputs_found >= jobs$expected_outputs, "complete", "incomplete")
)

print(
  jobs[c("stage", "job", "outputs_found", "expected_outputs", "status")],
  row.names = FALSE
)
