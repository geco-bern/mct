# User-selectable climate inputs and run-specific output naming.

input_config_path <- function(path = Sys.getenv(
  "MCT_INPUT_CONFIG",
  "config/input_sources.R"
)) {
  path.expand(path)
}

normalise_input_id <- function(id, field) {
  if (!is.character(id) || length(id) != 1L || !nzchar(id)) {
    stop(field, " must be one non-empty character value.", call. = FALSE)
  }
  value <- tolower(gsub("[^A-Za-z0-9._-]+", "-", trimws(id)))
  value <- gsub("^-+|-+$", "", value)
  if (!nzchar(value)) {
    stop(field, " does not contain a filename-safe identifier.", call. = FALSE)
  }
  value
}

validate_grid_config <- function(grid, field) {
  required <- c(
    "longitude_start", "longitude_step", "longitude_count",
    "latitude_start", "latitude_step", "latitude_count"
  )
  missing <- setdiff(required, names(grid))
  if (length(missing)) {
    stop(field, " is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (grid$longitude_step <= 0 || grid$latitude_step <= 0 ||
      grid$longitude_count < 1 || grid$latitude_count < 1) {
    stop(field, " grid steps and counts must be positive.", call. = FALSE)
  }
  invisible(grid)
}

validate_source_config <- function(source, field, require_conversion = FALSE) {
  required <- c(
    "netcdf_dir", "netcdf_pattern", "recursive", "tidy_dir", "tidy_prefix",
    "variable", "longitude_name", "latitude_name", "time_name", "scale",
    "offset", "grid"
  )
  if (require_conversion) required <- c(required, "conversion")
  missing <- setdiff(required, names(source))
  if (length(missing)) {
    stop(field, " is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  validate_grid_config(source$grid, paste0(field, "$grid"))
  invisible(source)
}

same_grid <- function(left, right) {
  grid_fields <- c(
    "longitude_start", "longitude_step", "longitude_count",
    "latitude_start", "latitude_step", "latitude_count"
  )
  isTRUE(all.equal(
    unlist(left$grid[grid_fields], use.names = FALSE),
    unlist(right$grid[grid_fields], use.names = FALSE),
    check.attributes = FALSE
  ))
}

read_input_config <- function(path = input_config_path()) {
  if (!file.exists(path)) {
    stop("Input configuration does not exist: ", path, call. = FALSE)
  }
  config <- dget(path)
  if (!is.list(config)) {
    stop("Input configuration must evaluate to one list.", call. = FALSE)
  }

  required <- c("analysis_period", "et", "precipitation", "temperature")
  missing <- setdiff(required, names(config))
  if (length(missing)) {
    stop("Input configuration is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  config$et$id <- normalise_input_id(config$et$id, "et$id")
  config$precipitation$id <- normalise_input_id(
    config$precipitation$id,
    "precipitation$id"
  )
  config$temperature$id <- normalise_input_id(
    config$temperature$id,
    "temperature$id"
  )

  validate_source_config(config$et$source, "et$source", require_conversion = TRUE)
  validate_source_config(
    config$et$low_resolution_source,
    "et$low_resolution_source",
    require_conversion = TRUE
  )
  et_conversions <- c(
    config$et$source$conversion,
    config$et$low_resolution_source$conversion
  )
  unsupported_conversions <- setdiff(
    et_conversions,
    c("latent_energy_to_mm", "identity_mm_day")
  )
  if (length(unsupported_conversions)) {
    stop(
      "Unsupported ET conversion: ",
      paste(unique(unsupported_conversions), collapse = ", "),
      call. = FALSE
    )
  }
  validate_source_config(config$precipitation$rain, "precipitation$rain")
  validate_source_config(config$precipitation$snow, "precipitation$snow")
  validate_source_config(config$temperature$source, "temperature$source")
  if (!same_grid(config$precipitation$rain, config$precipitation$snow) ||
      !same_grid(config$precipitation$rain, config$temperature$source)) {
    stop(
      "precipitation$rain, precipitation$snow, and temperature$source ",
      "must use the same grid.",
      call. = FALSE
    )
  }
  if (!same_grid(config$et$low_resolution_source, config$precipitation$rain)) {
    stop(
      "et$low_resolution_source must use the precipitation grid.",
      call. = FALSE
    )
  }

  years <- unlist(config$analysis_period[c("start_year", "end_year")])
  if (length(years) != 2L || anyNA(as.integer(years)) || years[[1]] > years[[2]]) {
    stop("analysis_period must define ordered start_year and end_year.", call. = FALSE)
  }
  config$analysis_period$start_year <- as.integer(years[[1]])
  config$analysis_period$end_year <- as.integer(years[[2]])
  config
}

climate_run_id <- function(config = read_input_config()) {
  paste0("et-", config$et$id, "__prec-", config$precipitation$id)
}

climate_output_path <- function(path, config = read_input_config()) {
  tag <- climate_run_id(config)
  dirname_path <- dirname(path)
  filename <- basename(path)
  extension <- tools::file_ext(filename)
  stem <- if (nzchar(extension)) {
    substr(filename, 1L, nchar(filename) - nchar(extension) - 1L)
  } else {
    filename
  }
  if (endsWith(stem, paste0("__", tag))) return(path)
  tagged <- paste0(
    stem, "__", tag,
    if (nzchar(extension)) paste0(".", extension) else ""
  )
  if (identical(dirname_path, ".")) tagged else file.path(dirname_path, tagged)
}

source_grid_values <- function(source, axis = c("longitude", "latitude")) {
  axis <- match.arg(axis)
  start <- source$grid[[paste0(axis, "_start")]]
  step <- source$grid[[paste0(axis, "_step")]]
  count <- as.integer(source$grid[[paste0(axis, "_count")]])
  start + step * seq.int(0L, count - 1L)
}

source_coordinate_digits <- function(source, axis = c("longitude", "latitude")) {
  axis <- match.arg(axis)
  step <- source$grid[[paste0(axis, "_step")]]
  max(0L, ceiling(-log10(step)) + 1L)
}

source_longitude_index <- function(longitude, source) {
  position <- (longitude - source$grid$longitude_start) /
    source$grid$longitude_step
  index <- floor(position + 0.5) + 1L
  as.integer(pmin(pmax(index, 1L), source$grid$longitude_count))
}

nearest_source_indices <- function(indices, from_source, to_source) {
  if (any(indices < 1L | indices > from_source$grid$longitude_count)) {
    stop("Longitude index is outside the source grid.", call. = FALSE)
  }
  longitude <- from_source$grid$longitude_start +
    from_source$grid$longitude_step * (indices - 1L)
  source_longitude_index(longitude, to_source)
}

nearest_source_index <- function(index, from_source, to_source) {
  nearest_source_indices(index, from_source, to_source)[[1]]
}

source_tidy_path <- function(source, longitude_index, config = read_input_config()) {
  untagged <- file.path(
    path.expand(source$tidy_dir),
    paste0(source$tidy_prefix, "_ilon_", longitude_index, ".RData")
  )
  climate_output_path(untagged, config)
}

source_netcdf_files <- function(source) {
  list.files(
    path.expand(source$netcdf_dir),
    pattern = source$netcdf_pattern,
    recursive = isTRUE(source$recursive),
    full.names = TRUE
  )
}

transform_input_values <- function(x, source) {
  x * as.numeric(source$scale) + as.numeric(source$offset)
}

et_to_w_m2 <- function(x, source) {
  converted <- transform_input_values(x, source)
  if (identical(source$conversion, "latent_energy_to_mm")) {
    converted / 86400
  } else if (identical(source$conversion, "identity_mm_day")) {
    converted * 2.45e6 / 86400
  } else {
    stop("Unsupported ET conversion: ", source$conversion, call. = FALSE)
  }
}
