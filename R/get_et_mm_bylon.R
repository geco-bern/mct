get_et_mm_byilon <- function(ilon_hires,
                             config = read_input_config(),
                             resolution = c("high", "low")) {
  resolution <- match.arg(resolution)
  find_lat_lores <- function(lat_hires, vec_lat_lores) {
    vec_lat_lores[which.min(abs(lat_hires - vec_lat_lores))]
  }

  source("R/convert_et.R")

  et_source <- if (resolution == "high") {
    config$et$source
  } else {
    config$et$low_resolution_source
  }
  temperature_source <- config$temperature$source
  output_directory <- if (resolution == "high") "df_et_mm" else "df_et_mm_lores"
  output <- climate_output_path(
    paste0("data/", output_directory, "/df_et_mm_ilon_", ilon_hires, ".RData"),
    config
  )
  ensure_directory(dirname(output))

  if (file.exists(output)) {
    message("File exists already: ", output)
    return(0)
  }

  if (ilon_hires < 1L || ilon_hires > et_source$grid$longitude_count) {
    stop("ET longitude index is outside the configured grid: ", ilon_hires, call. = FALSE)
  }

  ilon_lores <- nearest_source_index(
    ilon_hires,
    from_source = et_source,
    to_source = temperature_source
  )
  et_path <- source_tidy_path(et_source, ilon_hires, config)
  require_files(et_path, "ET conversion")
  load(et_path) # loads 'df'
  coordinate_digits <- source_coordinate_digits(et_source, "longitude")
  forcing_digits <- source_coordinate_digits(temperature_source, "longitude")
  df_alexi <- df %>%
    mutate(
      lon = round(lon, digits = coordinate_digits),
      lat = round(lat, digits = coordinate_digits),
      data = purrr::map(
        data,
        ~dplyr::rename(.x, et = tidyselect::all_of(et_source$variable))
      )
    )
  rm("df")

  vec_lat_lores <- source_grid_values(temperature_source, "latitude")
  df_alexi <- df_alexi %>%
    mutate(
      lon_lores = round(
        source_grid_values(temperature_source, "longitude")[[ilon_lores]],
        digits = forcing_digits
      ),
      lat_lores = purrr::map_dbl(
        lat,
        find_lat_lores,
        vec_lat_lores = vec_lat_lores
      ) %>% round(digits = forcing_digits)
    )

  if (identical(et_source$conversion, "latent_energy_to_mm")) {
    temperature_path <- source_tidy_path(temperature_source, ilon_lores, config)
    require_files(temperature_path, "ET conversion")
    load(temperature_path) # loads 'df'
    df_watch <- df %>%
      mutate(
        lon = round(lon, digits = forcing_digits),
        lat = round(lat, digits = forcing_digits),
        data = purrr::map(
          data,
          ~.x %>%
            dplyr::filter(
              lubridate::year(time) >= config$analysis_period$start_year,
              lubridate::year(time) <= config$analysis_period$end_year
            ) %>%
            dplyr::rename(temp = tidyselect::all_of(temperature_source$variable)) %>%
            dplyr::mutate(temp = transform_input_values(temp, temperature_source))
        )
      )
    rm("df")

    df_elv <- rbeni::extract_pointdata_allsites(
      path.expand(config$elevation_file),
      dplyr::select(df_alexi, lon, lat),
      time = FALSE
    ) %>%
      rename(elv = ETOPO1_Bed_g_geotiff) %>%
      mutate(
        lon = round(lon, digits = coordinate_digits),
        lat = round(lat, digits = coordinate_digits)
      )

    df_alexi <- df_alexi %>%
      left_join(
        df_watch %>%
          rename(lon_lores = lon, lat_lores = lat, data_watch = data),
        by = c("lon_lores", "lat_lores")
      ) %>%
      dplyr::filter(!is.na(lon_lores), !purrr::map_lgl(data_watch, is.null)) %>%
      mutate(data = purrr::map2(data, data_watch, ~right_join(.x, .y, by = "time"))) %>%
      dplyr::select(-data_watch) %>%
      left_join(df_elv, by = c("lon", "lat")) %>%
      mutate(
        data = purrr::map2(data, elv, ~mutate(.x, elv = .y)),
        data = purrr::map(
          data,
          ~mutate(.x, et = transform_input_values(et, et_source))
        ),
        data_et_mm = purrr::map(
          data,
          ~convert_et(.x$et, .x$temp, .x$elv, return_df = TRUE)
        ),
        data = purrr::map2(data, data_et_mm, bind_cols)
      ) %>%
      dplyr::select(-data_et_mm)
  } else if (identical(et_source$conversion, "identity_mm_day")) {
    df_alexi <- df_alexi %>%
      mutate(
        data = purrr::map(
          data,
          ~.x %>%
            dplyr::filter(
              lubridate::year(time) >= config$analysis_period$start_year,
              lubridate::year(time) <= config$analysis_period$end_year
            ) %>%
            dplyr::mutate(
              et = transform_input_values(et, et_source),
              et_mm = et
            )
        )
      )
  } else {
    stop(
      "Unsupported ET conversion: ", et_source$conversion,
      ". Use latent_energy_to_mm or identity_mm_day.",
      call. = FALSE
    )
  }

  message("Writing file: ", output)
  save(df_alexi, file = output)
  0
}
