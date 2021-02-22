# 15 Distribution model data for metapopulation capacity models


source("R/spartan/spartan_settings.R")

library(raster)
library(dplyr)
library(sf)
library(lubridate)
library(tibble)
library(magrittr)
library(tidyr)

load(file = "output/RData/00_controls_eg.RData")
load(file = "output/RData/01_landscape_variables_eg.RData")
load(file = "output/RData/07_combined_variables_eg.RData")
load(file = "output/RData/14.1_sp_occ_metapop_eg.RData")


source.functions("R/functions")



# ----------------------------

md_set <- bind_cols(
  varset_mpc_eg[1,],
  pa_data_eg_model
)


md <- md_set %$%
  mapply(
    FUN = get.model.data,
    x = pa_dat,
    y = all_vars,
    na.omit = FALSE,
    SIMPLIFY = FALSE
  )



distribution_model_data_mpc_eg <- pa_data_eg_model[,1:4] %>%
  bind_cols(
    tibble(
      dist_mod_dat = md
    )
  )

save(
  distribution_model_data_mpc_eg,
  file = "output/RData/15_distribution_model_data_mpc_eg.RData"
)