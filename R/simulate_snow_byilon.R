simulate_snow_byilon <- function(ilon, config = read_input_config()) {
  source("R/simulate_snow2.R")

  temperature_source <- config$temperature$source
  rain_source <- config$precipitation$rain
  snow_source <- config$precipitation$snow
  output <- climate_output_path(
    paste0("data/df_snow/df_snow_ilon_", ilon, ".RData"),
    config
  )
  ensure_directory(dirname(output))

  if (file.exists(output)) {
    message("File exists already: ", output)
    return(0)
  }

  input_paths <- c(
    source_tidy_path(temperature_source, ilon, config),
    source_tidy_path(rain_source, ilon, config),
    source_tidy_path(snow_source, ilon, config)
  )
  require_files(input_paths, "snow simulation")

  load(input_paths[[1]]) # loads 'df'
  df_temp <- df
  load(input_paths[[2]]) # loads 'df'
  df_prec <- df
  load(input_paths[[3]]) # loads 'df'
  df_snow <- df
  rm("df")

  within_period <- function(data) {
    dplyr::filter(
      data,
      lubridate::year(time) >= config$analysis_period$start_year,
      lubridate::year(time) <= config$analysis_period$end_year
    )
  }

  df_temp <- df_temp %>%
    ungroup() %>%
    mutate(
      data = purrr::map(data, within_period),
      data = purrr::map(
        data,
        ~.x %>%
          dplyr::rename(temp = tidyselect::all_of(temperature_source$variable)) %>%
          dplyr::mutate(temp = transform_input_values(temp, temperature_source))
      )
    )

  df_prec <- df_prec %>%
    ungroup() %>%
    mutate(
      data = purrr::map(data, within_period),
      data = purrr::map(
        data,
        ~.x %>%
          dplyr::rename(prec = tidyselect::all_of(rain_source$variable)) %>%
          dplyr::mutate(prec = transform_input_values(prec, rain_source))
      )
    )

  df_snow <- df_snow %>%
    ungroup() %>%
    mutate(
      data = purrr::map(data, within_period),
      data = purrr::map(
        data,
        ~.x %>%
          dplyr::rename(snow = tidyselect::all_of(snow_source$variable)) %>%
          dplyr::mutate(snow = transform_input_values(snow, snow_source))
      )
    )

  df <- df_temp %>%
    left_join(df_prec %>% rename(data_prec = data), by = c("lon", "lat")) %>%
    mutate(data = purrr::map2(data, data_prec, ~left_join(.x, .y, by = "time"))) %>%
    dplyr::select(-data_prec) %>%
    left_join(df_snow %>% rename(data_snow = data), by = c("lon", "lat")) %>%
    mutate(data = purrr::map2(data, data_snow, ~left_join(.x, .y, by = "time"))) %>%
    dplyr::select(-data_snow) %>%
    mutate(
      data = purrr::map(data, ~dplyr::select(.x, time, temp, prec, snow)),
      data = purrr::map(data, ~tidyr::drop_na(.x, time)),
      data = purrr::map(data, simulate_snow)
    )

  message("Writing file: ", output)
  save(df, file = output)
  0
}
