library(dplyr)
library(rbeni)
library(tidyr)
library(purrr)
library(ncdf4)
library(lubridate)
library(extRemes)

source("R/mct.R")
source("R/get_plantwhc_mct_bysite.R")

## Invoke all at once.
df <- get_plantwhc_mct_global()
