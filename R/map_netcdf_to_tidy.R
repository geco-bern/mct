# Convert NetCDF stacks with {map2tidy} while preserving legacy file contracts.

normalise_tidy_time <- function(df) {
  df$data <- lapply(df$data, function(data) {
    if ("datetime" %in% names(data) && !"time" %in% names(data)) {
      names(data)[names(data) == "datetime"] <- "time"
    }
    if ("time" %in% names(data) && is.character(data$time)) {
      parsed <- suppressWarnings(as.Date(data$time))
      if (sum(!is.na(parsed)) == sum(!is.na(data$time))) {
        data$time <- parsed
      }
    }
    data
  })
  df
}

map_netcdf_to_tidy <- function(nclist,
                               outdir,
                               fileprefix,
                               varnam,
                               lonnam = "lon",
                               latnam = "lat",
                               timenam = NA,
                               chunk = 1L,
                               chunks = 1L,
                               ilon = NULL,
                               ncores = allocated_cores(),
                               overwrite = FALSE,
                               fgetdate = NA) {
  if (!requireNamespace("map2tidy", quietly = TRUE)) {
    stop(
      "Package {map2tidy} is required. See README.md for installation instructions.",
      call. = FALSE
    )
  }
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    stop("Package {ncdf4} is required to read the longitude grid.", call. = FALSE)
  }

  nclist <- sort(path.expand(nclist))
  if (!length(nclist)) {
    stop("No NetCDF input files matched the configured input list.", call. = FALSE)
  }
  require_files(nclist, step = "NetCDF-to-tidy conversion")
  ensure_directory(path.expand(outdir))

  nc <- ncdf4::nc_open(nclist[[1]])
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  longitude <- as.numeric(ncdf4::ncvar_get(nc, lonnam))

  if (is.null(ilon)) {
    ilon <- seq_along(longitude)
  }
  if (any(ilon < 1L | ilon > length(longitude))) {
    stop("Requested longitude indices fall outside the NetCDF grid.", call. = FALSE)
  }

  selected <- work_for_chunk(ilon, chunk, chunks)
  if (!length(selected)) {
    message("No longitude bands assigned to this chunk.")
    return(invisible(character()))
  }

  output_paths <- file.path(
    path.expand(outdir),
    paste0(fileprefix, "_ilon_", selected, ".RData")
  )
  todo <- selected[overwrite | !file.exists(output_paths)]
  if (!length(todo)) {
    message("All outputs assigned to this chunk already exist.")
    return(invisible(output_paths))
  }

  temporary_dir <- tempfile(pattern = "map2tidy-")
  dir.create(temporary_dir)
  on.exit(unlink(temporary_dir, recursive = TRUE, force = TRUE), add = TRUE)

  resolution <- if (length(longitude) > 1L) {
    min(abs(diff(sort(unique(longitude)))), na.rm = TRUE)
  } else {
    0
  }
  longitude_filter <- range(longitude[todo]) + c(-resolution, resolution) / 3

  map2tidy::map2tidy(
    nclist = nclist,
    varnam = varnam,
    lonnam = lonnam,
    latnam = latnam,
    timenam = timenam,
    do_chunks = TRUE,
    na.rm = TRUE,
    outdir = temporary_dir,
    fileprefix = fileprefix,
    ncores = min(allocated_cores(length(todo)), ncores),
    fgetdate = fgetdate,
    overwrite = TRUE,
    filter_lon_between_degrees = longitude_filter
  )

  rds_files <- list.files(temporary_dir, pattern = "[.]rds$", full.names = TRUE)
  if (!length(rds_files)) {
    stop("{map2tidy} did not create any longitude-band files.", call. = FALSE)
  }

  written <- vapply(rds_files, function(path) {
    df <- normalise_tidy_time(readRDS(path))
    if (!all(c("lon", "lat", "data") %in% names(df))) {
      stop("Unexpected {map2tidy} output structure in ", path, call. = FALSE)
    }

    longitude_value <- unique(df$lon)
    if (length(longitude_value) != 1L) {
      stop("Expected one longitude band in ", path, call. = FALSE)
    }
    longitude_index <- which.min(abs(longitude - longitude_value))
    if (!longitude_index %in% todo) {
      return(NA_character_)
    }

    output <- file.path(
      path.expand(outdir),
      paste0(fileprefix, "_ilon_", longitude_index, ".RData")
    )
    write_rdata_atomic(df, "df", output)
    message("Wrote ", output)
    output
  }, character(1))

  written <- stats::na.omit(written)
  missing_indices <- setdiff(todo, vapply(written, function(path) {
    as.integer(sub(".*_ilon_([0-9]+)[.]RData$", "\\1", path))
  }, integer(1)))
  if (length(missing_indices)) {
    warning(
      "No output was produced for longitude indices: ",
      paste(missing_indices, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(unname(written))
}
