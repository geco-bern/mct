ingest_wtd_fan <- function(df){

  ## identify continent and set corresponding file name
  df_cont_filn <- tibble(
    cont = c("africa", "australia", "eurasia", "namerica", "samerica"),
    filn = c(
      "Africa_model_wtd_v2.nc",
      "Australia_model_wtd_v2.nc",
      "Eurasia_model_wtd_v2.nc",
      "N_America_model_wtd_v2.nc",
      "S_America_model_wtd_v2.nc")
    )

  df_cont <- df %>% 
    rowwise() %>% 
    mutate(cont = get_continent(lon, lat)) %>%
    left_join(
      df_cont_filn,
      by = "cont"
      )

  ## subset data frame and get WTD by continent
  rasta_africa <- raster::raster("~/data/watertable_fan13sci/Africa_model_wtd_v2.nc")
  rasta_australia <- raster::raster("~/data/watertable_fan13sci/Australia_model_wtd_v2.nc")
  rasta_eurasia <- raster::raster("~/data/watertable_fan13sci/Eurasia_model_wtd_v2.nc")
  rasta_namerica <- raster::raster("~/data/watertable_fan13sci/N_America_model_wtd_v2.nc")
  rasta_samerica <- raster::raster("~/data/watertable_fan13sci/S_America_model_wtd_v2.nc")

  ## start with extracting values for africa
  df_cont <- raster::extract(
      rasta_africa, 
      sp::SpatialPoints(
        df_cont %>% 
          dplyr::filter(cont == "africa") %>% # , proj4string = rasta@crs
          dplyr::select(lon, lat)),
      sp = TRUE ) %>%
    as_tibble() %>%
    setNames(c("wtd", "lon", "lat")) %>% 

    ## australia
    bind_rows(
      raster::extract(
        rasta_australia, 
        sp::SpatialPoints(
          df_cont %>% 
            dplyr::filter(cont == "australia") %>% # , proj4string = rasta@crs
            dplyr::select(lon, lat)),
        sp = TRUE ) %>%
      as_tibble() %>%
      setNames(c("wtd", "lon", "lat"))
    ) %>% 

    ## eurasia
    bind_rows(
      raster::extract(
        rasta_eurasia, 
        sp::SpatialPoints(
          df_cont %>% 
            dplyr::filter(cont == "eurasia") %>% # , proj4string = rasta@crs
            dplyr::select(lon, lat)),
        sp = TRUE ) %>%
      as_tibble() %>%
      setNames(c("wtd", "lon", "lat"))
    ) %>% 

    ## namerica
    bind_rows(
      raster::extract(
        rasta_namerica, 
        sp::SpatialPoints(
          df_cont %>% 
            dplyr::filter(cont == "namerica") %>% # , proj4string = rasta@crs
            dplyr::select(lon, lat)),
        sp = TRUE ) %>%
      as_tibble() %>%
      setNames(c("wtd", "lon", "lat"))
    ) %>% 

    ## samerica
    bind_rows(
      raster::extract(
        rasta_samerica, 
        sp::SpatialPoints(
          df_cont %>% 
            dplyr::filter(cont == "samerica") %>% # , proj4string = rasta@crs
            dplyr::select(lon, lat)),
        sp = TRUE ) %>%
      as_tibble() %>%
      setNames(c("wtd", "lon", "lat"))
    ) %>%
    distinct() %>%
    mutate(wtd = ifelse(wtd == 0.0, NA, wtd))
  
    ## put back into original order
    df <- df %>% 
      left_join(df_cont, by = c("lon", "lat"))
  
    return(df)
}

get_continent <- function(lon, lat){
  if (is_namerica(lon, lat)){
    cont <- "namerica"
  } else if (is_samerica(lon, lat)){
    cont <- "samerica"
  } else if (is_australia(lon, lat)){
    cont <- "australia"
  } else if (is_africa(lon, lat)){
    cont <- "africa"
  } else if (is_eurasia(lon, lat)){
    cont <- "eurasia"
  } else {
    cont <- NA
  }
  return(cont)
}


is_eurasia <- function(lon, lat){
  if (-14.0 < lon && lon < 180 && lat > -10 && lat < 83){
    out <- TRUE
  } else {
    out <- FALSE
  }
  return(out)
}

is_africa <- function(lon, lat){
  if (-19 < lon && lon < 55 && lat > -35 && lat < 38){
    out <- TRUE
  } else {
    out <- FALSE
  }
  return(out)
}

is_samerica <- function(lon, lat){
  if (-93 < lon && lon < -32 && lat > -56 && lat < 15){
    out <- TRUE
  } else {
    out <- FALSE
  }
  return(out)
}

is_namerica <- function(lon, lat){
  if (-180 < lon && lon < -52 && lat > 5 && lat < 84){
    out <- TRUE
  } else {
    out <- FALSE
  }
  return(out)
}

is_australia <- function(lon, lat){
  if (112 < lon && lon < 180 && lat > -56 && lat < -10){
    out <- TRUE
  } else {
    out <- FALSE
  }
  return(out)
}

