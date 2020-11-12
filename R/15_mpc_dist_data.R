# 15 Distribution model data for metapopulation capacity models


source("R/spartan/spartan_settings.R")

library(raster)
library(dplyr)
library(sf)
library(lubridate)
library(tibble)
library(magrittr)
library(tidyr)

load(file = "output/RData/00_controls.RData")
load(file = "output/RData/01_landscape_variables.RData")
load(file = "output/RData/07a_varset_mpc.RData")
load(file = "output/RData/14.1_sp_occ_metapop.RData")


source.functions("R/functions")

# ----------------------------

md_set <- bind_cols(
  varset_mpc[1,],
  pa_data_ch_model
)


md <- md_set %$%
  mapply(
    FUN = get.model.data,
    x = pa_dat,
    y = all_vars,
    na.omit = FALSE,
    SIMPLIFY = FALSE
  )



distribution_model_data_mpc <- pa_data_ch_model[,1:4] %>%
  bind_cols(
    tibble(
      dist_mod_dat = md
    )
  )

save(
  distribution_model_data_mpc,
  file = "output/RData/15_distribution_model_data_mpc.RData"
)