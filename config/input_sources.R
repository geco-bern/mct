# Climate-input namelist.
#
# Edit IDs, paths, variable names, transformations, and regular grids here;
# analysis scripts should not need source-specific edits. Values are transformed
# as `value * scale + offset`. ET conversion can be "latent_energy_to_mm" or
# "identity_mm_day". ET and precipitation IDs are embedded in output names.

list(
  analysis_period = list(
    start_year = 2003L,
    end_year = 2017L
  ),

  et = list(
    id = "alexi",
    source = list(
      netcdf_dir = "~/data/alexi_tir/netcdf",
      netcdf_pattern = "^EDAY_CERES_.*[.]nc$",
      recursive = FALSE,
      tidy_dir = "~/data/alexi_tir/data_tidy",
      tidy_prefix = "EDAY_CERES_",
      variable = "et",
      longitude_name = "lon",
      latitude_name = "lat",
      time_name = "time",
      conversion = "latent_energy_to_mm",
      scale = 1e6,
      offset = 0,
      grid = list(
        longitude_start = -179.975,
        longitude_step = 0.05,
        longitude_count = 7200L,
        latitude_start = -89.975,
        latitude_step = 0.05,
        latitude_count = 3600L
      )
    ),
    low_resolution_source = list(
      netcdf_dir = "~/data/alexi_tir/data_halfdeg",
      netcdf_pattern = "^EDAY_CERES_.*[.]nc$",
      recursive = FALSE,
      tidy_dir = "~/data/alexi_tir/data_tidy_halfdeg",
      tidy_prefix = "EDAY_CERES_",
      variable = "et",
      longitude_name = "lon",
      latitude_name = "lat",
      time_name = "time",
      conversion = "latent_energy_to_mm",
      scale = 1e6,
      offset = 0,
      grid = list(
        longitude_start = -179.75,
        longitude_step = 0.5,
        longitude_count = 720L,
        latitude_start = -89.75,
        latitude_step = 0.5,
        latitude_count = 360L
      )
    )
  ),

  precipitation = list(
    id = "watch-wfdei",
    rain = list(
      netcdf_dir = "~/data/watch_wfdei",
      netcdf_pattern = "Rainf_daily_WFDEI_CRU_.*[.]nc$",
      recursive = TRUE,
      tidy_dir = "~/data/watch_wfdei/data_tidy",
      tidy_prefix = "Rainf_daily_WFDEI_CRU_",
      variable = "Rainf",
      longitude_name = "lon",
      latitude_name = "lat",
      time_name = "timestp",
      scale = 86400,
      offset = 0,
      grid = list(
        longitude_start = -179.75,
        longitude_step = 0.5,
        longitude_count = 720L,
        latitude_start = -89.75,
        latitude_step = 0.5,
        latitude_count = 360L
      )
    ),
    snow = list(
      netcdf_dir = "~/data/watch_wfdei",
      netcdf_pattern = "Snowf_daily_WFDEI_CRU_.*[.]nc$",
      recursive = TRUE,
      tidy_dir = "~/data/watch_wfdei/data_tidy",
      tidy_prefix = "Snowf_daily_WFDEI_CRU_",
      variable = "Snowf",
      longitude_name = "lon",
      latitude_name = "lat",
      time_name = "timestp",
      scale = 86400,
      offset = 0,
      grid = list(
        longitude_start = -179.75,
        longitude_step = 0.5,
        longitude_count = 720L,
        latitude_start = -89.75,
        latitude_step = 0.5,
        latitude_count = 360L
      )
    )
  ),

  temperature = list(
    id = "watch-wfdei",
    source = list(
      netcdf_dir = "~/data/watch_wfdei",
      netcdf_pattern = "Tair_daily_WFDEI_.*[.]nc$",
      recursive = TRUE,
      tidy_dir = "~/data/watch_wfdei/data_tidy",
      tidy_prefix = "Tair_daily_WFDEI_",
      variable = "Tair",
      longitude_name = "lon",
      latitude_name = "lat",
      time_name = "timestp",
      scale = 1,
      offset = -273.15,
      grid = list(
        longitude_start = -179.75,
        longitude_step = 0.5,
        longitude_count = 720L,
        latitude_start = -89.75,
        latitude_step = 0.5,
        latitude_count = 360L
      )
    )
  ),

  elevation_file = "~/data/etopo/ETOPO1_Bed_g_gef_0.05deg_STANDARD.nc"
)
