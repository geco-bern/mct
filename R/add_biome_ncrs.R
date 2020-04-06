add_biome_ncrs <- function(df, path){
  
  rasta_biome <- raster::raster(path) %>% 
    subset(1, drop = TRUE)
  
  ## Extract data at points given by global model output (gridcells)
  df_biome_ncrs <- extract(rasta_biome, SpatialPoints(dplyr::select(df, lon, lat)), sp = TRUE) %>% 
    as_tibble()
  
  df <- df_biome_ncrs %>% 
    dplyr::select(-lat, -lon) %>% 
    bind_cols(df) %>% 
    rename(biome_ncrs = biomes) %>% 
    mutate(biome_ncrs = as.factor(biome_ncrs))
  
  biome_key <- tibble(
    biome_ncrs = as.factor(0:14),
    biome_ncrs_chr = c(
      "Ocean",
      "TUNDRA Permafrost",
      "TUNDRA Interfrost",
      "BOREAL Semi-arid",
      "BOREAL Humid",
      "TEMPERATE Semi-arid",
      "TEMPERATE Humid",
      "MEDITERRANEAN Warm",
      "MEDITERRANEAN Cold",
      "DESERT Tropical",
      "DESERT Temperate",
      "DESERT Cold",
      "TROPICAL Semi-arid",
      "TROPICAL Humid",
      "Ice"
    )
  )
  df <- df %>%
    left_join(biome_key, by = "biome_ncrs") 
  return(df)
}