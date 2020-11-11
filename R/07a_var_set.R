# 07a. smaller var_set

source("R/spartan/spartan_settings.R")

library(raster)
library(dplyr)
library(sf)
library(lubridate)
library(tibble)
library(magrittr)
library(tidyr)

load(file = "output/RData/07_combined_variables.RData")

varset_mpc <- var_set %>%
  filter(climate_model == "ACCESS1-0")

var_set_ids <- var_set %>%
  dplyr::select(
    "scenario",
    "scenario_replicate",
    "rcp",
    "climate_model",
    "harvest_scenario",
    "plan_burn",
    "scn_id",
    "th",
    "rc",
    "pb",
    "scn_no",
    "cscnid"
  )

save(
  varset_mpc,
  file = "output/RData/07a_varset_mpc.RData"
)

save(
  var_set_ids,
  file = "output/RData/07a_var_set_ids.RData"
)