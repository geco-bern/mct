library(dplyr)
library(rbeni)
library(tidyr)
library(purrr)
library(ncdf4)
library(lubridate)
library(extRemes)

source("R/mct.R")
source("R/get_plantwhc_mct_bysite.R")
source("R/get_plantwhc_mct_global.R")

## Invoke all at once.
df <- get_df_landmask()
df_test <- get_plantwhc_mct_global(df)
